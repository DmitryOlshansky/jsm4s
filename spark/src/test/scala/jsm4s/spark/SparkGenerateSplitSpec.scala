package jsm4s.spark

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import org.apache.spark.{SparkConf, SparkContext, TaskContext}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scala.collection.JavaConverters._

class SparkGenerateSplitSpec extends FunSuite with Matchers with BeforeAndAfterAll {
  private var sc: SparkContext = _
  private val temp = Files.createTempDirectory("jsm4s-spark-test-")
  override def beforeAll(): Unit = {
    sc = new SparkContext(new SparkConf().setMaster("local[2,2]").setAppName("split-test")
      .set("spark.ui.enabled", "false").set("spark.driver.host", "127.0.0.1")
      .set("spark.driver.bindAddress", "127.0.0.1"))
    sc.setLogLevel("ERROR")
  }
  override def afterAll(): Unit = {
    if (sc != null) sc.stop()
    val walk = Files.walk(temp)
    try walk.iterator.asScala.toVector.reverse.foreach(Files.deleteIfExists)
    finally walk.close()
  }
  test("distributed output is headerless, unique, readable, and cannot overwrite an existing run") {
    val input = temp.resolve("input.dat")
    Files.write(input, "# attributes: 2 properties: B(1,2)\n0 1 | 1\n0 1 | 1\n".getBytes(UTF_8))
    val output = temp.resolve("model")
    SparkGenerateSplit.run(sc, input.toUri.toString, output.toUri.toString, MiningOptions(2, 1, roundSteps = 1))
    Files.exists(output.resolve("_COMPLETE")) shouldBe true
    Files.exists(output.resolve("hypotheses/_SUCCESS")) shouldBe true
    new String(Files.readAllBytes(output.resolve("header.txt")), UTF_8) shouldEqual
      "# attributes: 2 properties: B(1,2)\n"
    val paths = Files.list(output.resolve("hypotheses"))
    val lines = try paths.iterator.asScala.filter(_.getFileName.toString.startsWith("part-"))
      .flatMap(p => Files.readAllLines(p, UTF_8).asScala).toVector finally paths.close()
    lines shouldEqual Vector("0 1 | 1")
    val rounds = Files.list(output.resolve("rounds"))
    val count = try rounds.iterator.asScala.count(_.getFileName.toString.startsWith("round-")) finally rounds.close()
    count should be > 1
    val committed = sc.objectFile[ShardRoundResult](output.resolve("rounds/round-*/part-*").toUri.toString).collect()
    committed.forall(_.progress.steps <= 1L) shouldBe true
    committed.flatMap(_.records).toVector shouldEqual Vector("0 1 | 1")
    committed.exists(r => r.state.done && r.progress.steps == 0) shouldBe true
    intercept[IllegalArgumentException] {
      SparkGenerateSplit.run(sc, input.toUri.toString, output.toUri.toString, MiningOptions(2, 1))
    }
  }

  test("a failed task attempt does not duplicate committed hypotheses") {
    val metadata = SplitMetadata("# attributes: 2 properties: B(1,2)", 2, Array(0, 1))
    val shard = SplitPayload(0, 0, 2, Array(Array(0, 1), Array(0, 1)), Array("1", "1"))
    val output = temp.resolve("retried")
    sc.parallelize(Seq(shard), 1).map { s =>
        val result = ShardRound.advance(s, metadata, MiningOptions(1, 1, roundSteps = 1), None)
        if (TaskContext.get().attemptNumber() == 0) {
          throw new RuntimeException("intentional retry test")
        }
        result
    }.saveAsObjectFile(output.toUri.toString)
    val committed = sc.objectFile[ShardRoundResult](output.toUri.toString).collect().toVector
    committed.flatMap(_.records) shouldEqual Vector("0 1 | 1")
    committed should have size 1
    val next = ShardRound.advance(shard, metadata, MiningOptions(1, 1), Some(committed.head.state))
    next.records shouldBe empty
    next.state.done shouldBe true
  }

  test("different round budgets produce the same model including uneven and empty shards") {
    val input = temp.resolve("uneven.dat")
    val body = "# attributes: 4 properties: B(1,2)\n0 2 | 1\n0 2 3 | 1\n2 3 | 2\n"
    Files.write(input, body.getBytes(UTF_8))
    val expected = new java.io.ByteArrayOutputStream
    jsm4s.JSM.generate(new java.io.ByteArrayInputStream(body.getBytes(UTF_8)), expected,
      "cbo", "dense", "noCounterExamples", 1.0, 1, 1)
    for (budget <- Seq(2L, 100L)) {
      val output = temp.resolve(s"budget-$budget")
      SparkGenerateSplit.run(sc, input.toUri.toString, output.toUri.toString, MiningOptions(4, 1, roundSteps = budget))
      val actual = sc.textFile(output.resolve("hypotheses").toUri.toString).collect().sorted.toVector
      actual shouldEqual expected.toString("UTF-8").split("\n").drop(1).sorted.toVector
    }
  }

  private def hybrid(body: String, name: String, options: MiningOptions): Vector[HybridRoundRecord] = {
    val input = temp.resolve(name + ".dat")
    val output = temp.resolve(name)
    Files.write(input, body.getBytes(UTF_8))
    SparkGenerateSplit.run(sc, input.toUri.toString, output.toUri.toString, options)
    Files.exists(output.resolve("_COMPLETE")) shouldBe true
    val expected = new java.io.ByteArrayOutputStream
    jsm4s.JSM.generate(new java.io.ByteArrayInputStream(body.getBytes(UTF_8)), expected,
      "cbo", "dense", options.strategy, 1.0, options.support, 1)
    val actual = sc.textFile(output.resolve("hypotheses").toUri.toString).collect().sorted.toVector
    actual shouldEqual expected.toString("UTF-8").split("\n").drop(1).sorted.toVector
    val records = sc.objectFile[HybridRoundRecord](output.resolve("rounds/round-*/part-*").toUri.toString)
      .collect().toVector
    records.flatMap(_.records).sorted shouldEqual actual
    val batches = records.collect { case VerticalCommit(batch, _) => batch }
    batches.foreach { batch =>
      batch.candidates.size should be <= options.candidateBatchSize
      batch.progress.steps should be <= options.roundSteps
    }
    val candidates = batches.flatMap(_.candidates)
    candidates.map(_.candidate.id).sorted shouldEqual candidates.indices.map(_.toLong).toVector
    val decisions = records.collect { case d: VerificationCommit => d }
    decisions.map(_.candidateId).sorted shouldEqual
      candidates.filter(_.requiresHorizontalCheck).map(_.candidate.id).sorted
    val checks = records.collect { case h: HorizontalCommit => h }.flatMap(_.checks)
    checks.size shouldEqual decisions.size * options.splits
    checks.groupBy(_.candidateId).values.foreach { replies =>
      replies.map(_.shardId).sorted shouldEqual (0 until options.splits).toVector
    }
    val json = new String(Files.readAllBytes(output.resolve("metadata.json")), UTF_8)
    json should include ("\"formatVersion\": 3")
    json should include ("\"hypotheses\": " + actual.size)
    json should include ("\"verticalCandidates\": " + candidates.size)
    json should include ("\"horizontalChecks\": " + checks.size)
    intercept[IllegalArgumentException] {
      SparkGenerateSplit.run(sc, input.toUri.toString, output.toUri.toString, options)
    }
    records
  }

  test("hybrid rounds preserve CLI output across cuts, budgets and property strategies") {
    val body = "# attributes: 3 properties: B(1,2)\n0 1 | 1\n0 2 | 2\n1 2 | 1\n1 2 | 1\n"
    val cases = Seq(
      MiningOptions(2, 1, roundSteps = 1, verticalCut = Some(0), candidateBatchSize = 1),
      MiningOptions(2, 1, roundSteps = 2, verticalCut = Some(1), candidateBatchSize = 1),
      MiningOptions(2, 1, roundSteps = 100, verticalCut = Some(1), candidateBatchSize = 100),
      MiningOptions(2, 1, roundSteps = 100, verticalCut = Some(3), candidateBatchSize = 1),
      MiningOptions(2, 1, strategy = "votingMajority", roundSteps = 2,
        verticalCut = Some(1), candidateBatchSize = 1),
      MiningOptions(2, 1, strategy = "boundedVotingMajority:67", roundSteps = 100,
        verticalCut = Some(2), candidateBatchSize = 2))
    cases.zipWithIndex.foreach { case (options, i) => hybrid(body, s"hybrid-$i", options) }
  }

  test("finished H miners still verify V and support shortcuts do not request H checks") {
    val body = "# attributes: 3 properties: B(1,2)\n0 1 | 1\n0 2 | 1\n1 2 | 1\n1 2 | 1\n"
    val records = hybrid(body, "finished-h", MiningOptions(2, 1, roundSteps = 100,
      verticalCut = Some(1), candidateBatchSize = 1))
    records.exists {
      case HorizontalCommit(mining, checks) => mining.state.done && mining.progress.steps == 0 && checks.nonEmpty
      case _ => false
    } shouldBe true
    records.collect { case VerticalCommit(_, direct) => direct }.flatten should not be empty
    records.collect { case d: VerificationCommit => d }.exists(_.accepted) shouldBe true
  }

  test("last V batch is drained after all miners finish and noncanonical candidates are rejected") {
    val body = "# attributes: 2 properties: B(1,2)\n0 1 | 1\n0 1 | 1\n"
    val records = hybrid(body, "drain", MiningOptions(2, 1, roundSteps = 100,
      verticalCut = Some(1), candidateBatchSize = 10))
    val batches = records.collect { case VerticalCommit(batch, _) => batch }
    batches should have size 2
    batches.forall(_.state.done) shouldBe true
    val decisions = records.collect { case d: VerificationCommit => d }
    decisions should have size 1
    decisions.head.accepted shouldBe false
    decisions.head.records shouldBe empty
  }

  test("hybrid empty data finishes with an empty model") {
    hybrid("# attributes: 2 properties: B(1,2)\n", "empty-hybrid",
      MiningOptions(2, 1, roundSteps = 100, verticalCut = Some(1))).flatMap(_.records) shouldBe empty
  }

  test("hybrid task retry preserves pending candidates, H replies and output without duplicates") {
    val metadata = SplitMetadata("# attributes: 2 properties: B(1,2)", 2, Array(0, 1))
    val payload = VerticalPayload(1, Array(Array(1), Array(1)), Array("1", "1"), 2)
    val shard = SplitPayload(0, 0, 1, Array(Array(0, 1), Array(0, 1)), Array("1", "1"))
    val options = MiningOptions(1, 1, roundSteps = 100, verticalCut = Some(1))
    val v = HybridRound.vertical(payload, metadata, options, None)
    val pending = v.batch.candidates.filter(_.requiresHorizontalCheck)
    val path = temp.resolve("hybrid-retried")
    sc.parallelize(Seq(0, 1), 2).map { id =>
      val result: HybridRoundRecord = if (id == 0)
        HybridRound.horizontal(shard, metadata, options, None, 1, pending)
      else HybridRound.vertical(payload, metadata, options, None)
      if (TaskContext.get().attemptNumber() == 0) throw new RuntimeException("intentional hybrid retry")
      result
    }.saveAsObjectFile(path.toUri.toString)
    val committed = sc.objectFile[HybridRoundRecord](path.toUri.toString).collect().toVector
    committed should have size 2
    committed.flatMap(_.records) shouldEqual Vector("0 1 | 1")
    val h = committed.collect { case x: HorizontalCommit => x }.head
    h.checks should have size 1
    val restored = committed.collect { case x: VerticalCommit => x }.head
    restored.batch.state.nextCandidateId shouldBe 1L
    restored.batch.candidates.map(_.candidate.id) shouldEqual Vector(0L)
    HybridRound.decide(restored.batch.candidates.head,
      Some(VerificationTally(1, h.checks.head.rejected)), 1, metadata).accepted shouldBe false
  }

  test("missing or duplicate H replies fail verification and hybrid options are validated") {
    val metadata = SplitMetadata("# attributes: 2 properties: B(1,2)", 2, Array(0, 1))
    val item = SupportedVerticalCandidate(VerticalCandidate(0, 1, new jsm4s.ds.BitInt(2).values(Seq(1))), 1, "1")
    intercept[IllegalArgumentException](HybridRound.decide(item, None, 2, metadata))
    intercept[IllegalArgumentException](HybridRound.decide(item, Some(VerificationTally(1, false)), 2, metadata))
    intercept[IllegalArgumentException](HybridRound.decide(item, Some(VerificationTally(3, false)), 2, metadata))
    intercept[IllegalArgumentException](MiningOptions(1, verticalCut = Some(-1)))
    intercept[IllegalArgumentException](MiningOptions(1, candidateBatchSize = 0))
    val cli = new Arguments(Seq("--input", "in", "--output", "out", "--splits", "2",
      "--vertical-cut", "1", "--candidate-batch-size", "7"))
    cli.verticalCut.toOption shouldEqual Some(1)
    cli.candidateBatchSize() shouldBe 7
  }
}
