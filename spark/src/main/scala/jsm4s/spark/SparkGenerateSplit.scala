package jsm4s.spark

import java.nio.charset.StandardCharsets.UTF_8
import java.security.{DigestInputStream, MessageDigest}
import jsm4s.FIMI
import jsm4s.ds.ArrayInt
import jsm4s.algorithm.CbOState
import org.apache.hadoop.fs.Path
import org.apache.spark.{HashPartitioner, SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.rogach.scallop._

private[spark] class Arguments(args: Seq[String]) extends ScallopConf(args) {
  val input = opt[String](required = true, descr = "Input FIMI path or Hadoop URI")
  val output = opt[String](required = true, descr = "New output directory; never overwritten")
  val splits = opt[Int](required = true)
  val support = opt[Int](default = Some(2))
  val algorithm = opt[String](default = Some("cbo"))
  val dataStructure = opt[String](name = "data-structure", default = Some("dense"))
  val strategy = opt[String](default = Some("noCounterExamples"))
  val sampling = opt[Double](default = Some(1.0))
  val roundSteps = opt[Long](name = "round-steps", default = Some(100000L),
    descr = "Maximum DFS transitions per shard per round (not a time limit)")
  val verticalCut = opt[Int](name = "vertical-cut",
    descr = "Enable H/V rounds: V owns sorted attributes from this position onward")
  val candidateBatchSize = opt[Int](name = "candidate-batch-size", default = Some(256),
    descr = "Maximum property-filtered V candidates produced per round")
  validate(algorithm)(value => if (value == "cbo") Right(())
    else Left("Only cbo is supported; Spark supplies parallelism"))
  validate(sampling)(value => if (value == 1.0) Right(())
    else Left("Only sampling=1 is supported for retry-stable output"))
  verify()
}

object SparkGenerateSplit {
  def main(args: Array[String]): Unit = {
    val cli = new Arguments(args.toSeq)
    val options = MiningOptions(cli.splits(), cli.support(), cli.dataStructure(), cli.strategy(), cli.roundSteps(),
      cli.verticalCut.toOption, cli.candidateBatchSize())
    val sc = new SparkContext(new SparkConf().setAppName("jsm4s-generate-split"))
    try run(sc, cli.input(), cli.output(), options)
    finally sc.stop()
  }

  def run(sc: SparkContext, input: String, output: String, options: MiningOptions): Unit = {
    val target = new Path(output)
    val fs = target.getFileSystem(sc.hadoopConfiguration)
    require(!fs.exists(target), s"Output already exists: $target")
    val inputPath = new Path(input)
    val digest = MessageDigest.getInstance("SHA-256")
    val stream = new DigestInputStream(inputPath.getFileSystem(sc.hadoopConfiguration).open(inputPath), digest)
    val data = try FIMI.load(stream, attrs => new ArrayInt(attrs)) finally stream.close()
    val inputHash = digest.digest().map(b => f"${b & 0xff}%02x").mkString
    if (options.verticalCut.isDefined) {
      val plan = VerticalPlan.prepare(data, options, options.verticalCut.get)
      HybridSparkRounds.run(sc, plan, options, output, inputHash)
      return
    }
    val plan = SplitPlan.prepare(data, options)
    val metadata = sc.broadcast(plan.metadata)
    val partitioner = new HashPartitioner(options.splits)
    val fixed = sc.parallelize(plan.shards.map(s => s.id -> s), options.splits)
      .partitionBy(partitioner).persist(StorageLevel.MEMORY_AND_DISK)
    var previous: Option[RDD[ShardRoundResult]] = None
    var rounds = 0
    var totalSteps = 0L
    var totalClosures = 0L
    var totalRecords = 0L
    def roundPath(index: Int): String = new Path(target, f"rounds/round-$index%06d").toString
    try {
      var active = true
      while (active) {
        val states: RDD[(Int, Option[CbOState])] = previous match {
          case None => fixed.mapValues(_ => Option.empty[CbOState])
          case Some(result) => result.map(r => r.id -> Some(r.state))
        }
        val outcomes = fixed.join(states, partitioner).map { case (_, (shard, state)) =>
          ShardRound.advance(shard, metadata.value, options, state)
        }
        // Reliable round boundary: output and next state are one committed dataset.
        // Reading it back truncates lineage without depending on executor cache survival.
        outcomes.saveAsObjectFile(roundPath(rounds))
        val committed = sc.objectFile[ShardRoundResult](roundPath(rounds))
          .persist(StorageLevel.MEMORY_AND_DISK)
        val old = previous
        previous = Some(committed)
        val summary = committed.map { r =>
          (if (r.state.done) 0L else 1L, r.progress.steps, r.progress.closures, r.records.size.toLong)
        }.fold((0L, 0L, 0L, 0L)) { (a, b) =>
          (a._1 + b._1, a._2 + b._2, a._3 + b._3, a._4 + b._4)
        }
        old.foreach(_.unpersist())
        rounds += 1
        totalSteps += summary._2
        totalClosures += summary._3
        totalRecords += summary._4
        active = summary._1 > 0
        System.err.println(s"CbO round $rounds: active=${summary._1}, steps=${summary._2}, hypotheses=${summary._4}")
      }
      // Preserve the existing public text output and collector contract.
      sc.union((0 until rounds).map(i => sc.objectFile[ShardRoundResult](roundPath(i))))
        .flatMap(_.records).saveAsTextFile(new Path(target, "hypotheses").toString)

      def write(name: String, content: String): Unit = {
        val out = fs.create(new Path(target, name), false)
        try out.write(content.getBytes(UTF_8)) finally out.close()
      }
      write("header.txt", plan.metadata.header + "\n")
      val sizes = plan.shards.map(_.rows.length).mkString("[", ",", "]")
      val order = plan.metadata.order.mkString("[", ",", "]")
      write("metadata.json", s"""{
  "formatVersion": 2,
  "algorithm": "cbo",
  "sparkVersion": "${org.apache.spark.SPARK_VERSION}",
  "inputSha256": "$inputHash",
  "splits": ${options.splits},
  "support": ${options.support},
  "strategy": "${options.strategy}",
  "dataStructure": "${options.dataStructure}",
  "sampling": 1.0,
  "roundSteps": ${options.roundSteps},
  "rounds": $rounds,
  "steps": $totalSteps,
  "closures": $totalClosures,
  "hypotheses": $totalRecords,
  "shardRowCounts": $sizes,
  "sortedToOriginalAttributes": $order
}
""")
      write("_COMPLETE", "")
    } finally {
      previous.foreach(_.unpersist())
      fixed.unpersist()
      metadata.destroy()
    }
  }
}
