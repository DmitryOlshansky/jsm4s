package jsm4s.spark

import java.nio.charset.StandardCharsets.UTF_8
import jsm4s.algorithm.CbOState
import org.apache.hadoop.fs.Path
import org.apache.spark.{HashPartitioner, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

object HybridSparkRounds {
  def run(sc: SparkContext, plan: VerticalPlan, options: MiningOptions,
          output: String, inputHash: String): Unit = {
    require(options.verticalCut.contains(plan.vertical.cut), "Vertical plan/options cut mismatch")
    val target = new Path(output)
    val fs = target.getFileSystem(sc.hadoopConfiguration)
    require(!fs.exists(target), s"Output already exists: $target")
    val metadata = sc.broadcast(plan.metadata)
    val cut = plan.vertical.cut
    val partitioner = new HashPartitioner(options.splits)
    val fixedH = sc.parallelize(plan.horizontal.map(s => s.id -> s), options.splits)
      .partitionBy(partitioner).persist(StorageLevel.MEMORY_AND_DISK)
    val fixedV = sc.parallelize(Seq(0 -> plan.vertical), 1).persist(StorageLevel.MEMORY_AND_DISK)
    var previous = Option.empty[RDD[HybridRoundRecord]]
    var rounds = 0
    var totals = HybridSummary()
    def roundPath(index: Int): String = new Path(target, f"rounds/round-$index%06d").toString
    try {
      var finished = false
      while (!finished) {
        // Only one bounded candidate batch visits the driver. DFS stacks, H replies,
        // and model records remain in RDDs. IDs are assigned by V, never by tasks.
        val pending = previous.map(_.flatMap {
          case VerticalCommit(batch, _) => batch.candidates.filter(_.requiresHorizontalCheck)
          case _ => Vector.empty[SupportedVerticalCandidate]
        }.collect().toVector).getOrElse(Vector.empty)
        require(pending.size <= options.candidateBatchSize, "Pending V batch exceeds configured bound")
        require(pending.map(_.candidate.id).distinct.size == pending.size, "Duplicate pending candidate IDs")
        val requests = sc.broadcast(pending)
        var workers = Option.empty[RDD[HybridRoundRecord]]
        try {
          val hStates: RDD[(Int, Option[CbOState])] = previous match {
            case None => fixedH.mapValues(_ => Option.empty[CbOState])
            case Some(committed) => committed.flatMap {
              case HorizontalCommit(mining, _) => Some(mining.id -> Some(mining.state))
              case _ => None
            }
          }
          val vStates: RDD[(Int, Option[VerticalState])] = previous match {
            case None => fixedV.mapValues(_ => Option.empty[VerticalState])
            case Some(committed) => committed.flatMap {
              case VerticalCommit(batch, _) => Some(0 -> Some(batch.state))
              case _ => None
            }
          }
          val horizontal: RDD[HybridRoundRecord] = fixedH.join(hStates, partitioner).map {
            case (_, (shard, state)) =>
              HybridRound.horizontal(shard, metadata.value, options, state, cut, requests.value)
          }
          val vertical: RDD[HybridRoundRecord] = fixedV.join(vStates, new HashPartitioner(1)).map {
            case (_, (payload, state)) => HybridRound.vertical(payload, metadata.value, options, state)
          }
          // The same worker stage computes V's next batch and H's previous-batch
          // checks plus mining. Cached results feed both decisions and the commit.
          val computed = horizontal.union(vertical).persist(StorageLevel.MEMORY_AND_DISK)
          workers = Some(computed)
          val decisions: RDD[HybridRoundRecord] = if (pending.isEmpty) sc.emptyRDD[HybridRoundRecord] else {
            val replies = computed.flatMap {
              case HorizontalCommit(_, checks) => checks.map(c =>
                c.candidateId -> VerificationTally(1, c.rejected))
              case _ => Vector.empty[(Long, VerificationTally)]
            }.reduceByKey(_ + _)
            sc.parallelize(pending, math.min(pending.size, options.splits)).map(i => i.candidate.id -> i)
              .leftOuterJoin(replies).map { case (_, (item, tally)) =>
                HybridRound.decide(item, tally, options.splits, metadata.value)
              }
          }
          computed.union(decisions).saveAsObjectFile(roundPath(rounds))
          val committed = sc.objectFile[HybridRoundRecord](roundPath(rounds))
            .persist(StorageLevel.MEMORY_AND_DISK)
          val old = previous
          previous = Some(committed)
          val summary = committed.map(r => HybridSummary(r)).fold(HybridSummary())(_ + _)
          old.foreach(_.unpersist())
          totals = totals + summary
          rounds += 1
          finished = summary.finished
          System.err.println(s"H/V round $rounds: activeH=${summary.activeH}, activeV=${summary.activeV}, " +
            s"pending=${summary.pending}, checks=${summary.checks}, hypotheses=${summary.hypotheses}")
        } finally {
          workers.foreach(_.unpersist())
          requests.destroy()
        }
      }
      // Finished includes draining V's last pending batch, even when both miners
      // have empty stacks. Each accepted candidate contributes one committed record.
      sc.union((0 until rounds).map(i => sc.objectFile[HybridRoundRecord](roundPath(i))))
        .flatMap(_.records).saveAsTextFile(new Path(target, "hypotheses").toString)
      def write(name: String, content: String): Unit = {
        val stream = fs.create(new Path(target, name), false)
        try stream.write(content.getBytes(UTF_8)) finally stream.close()
      }
      write("header.txt", plan.metadata.header + "\n")
      val sizes = plan.horizontal.map(_.rows.length).mkString("[", ",", "]")
      val order = plan.metadata.order.mkString("[", ",", "]")
      write("metadata.json", s"""{
  "formatVersion": 3,
  "mode": "horizontal-vertical",
  "algorithm": "cbo",
  "sparkVersion": "${org.apache.spark.SPARK_VERSION}",
  "inputSha256": "$inputHash",
  "splits": ${options.splits},
  "support": ${options.support},
  "strategy": "${options.strategy}",
  "dataStructure": "${options.dataStructure}",
  "sampling": 1.0,
  "roundSteps": ${options.roundSteps},
  "verticalCut": $cut,
  "candidateBatchSize": ${options.candidateBatchSize},
  "maxPrefixSupport": ${plan.vertical.maxPrefixSupport},
  "verticalRows": ${plan.vertical.rows.length},
  "rounds": $rounds,
  "steps": ${totals.steps},
  "closures": ${totals.closures},
  "hypotheses": ${totals.hypotheses},
  "verticalCandidates": ${totals.candidates},
  "horizontalChecks": ${totals.checks},
  "verifiedCandidates": ${totals.verified},
  "rejectedCandidates": ${totals.rejected},
  "shortcutCandidates": ${totals.shortcuts},
  "rejectedProperties": ${totals.rejectedProperties},
  "shardRowCounts": $sizes,
  "sortedToOriginalAttributes": $order
}
""")
      write("_COMPLETE", "")
    } finally {
      previous.foreach(_.unpersist())
      fixedH.unpersist()
      fixedV.unpersist()
      metadata.destroy()
    }
  }
}
