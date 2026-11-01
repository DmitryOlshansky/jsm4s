package jsm4s.spark

import java.io._
import jsm4s.{FIMI, JSM}
import jsm4s.algorithm.{Hypothesis, Sink}
import jsm4s.ds.ArrayInt
import jsm4s.property.BinaryProperty
import org.scalatest.{FunSuite, Matchers}

class SplitPlanSpec extends FunSuite with Matchers {
  private val factory = new BinaryProperty.Factory(Seq("1", "2"))
  private def data(rows: Seq[Set[Int]], attrs: Int): FIMI = {
    val ints = new ArrayInt(attrs)
    FIMI(rows.map(r => ints.values(r.toSeq.sorted)), rows.map(_ => BinaryProperty.Positive),
      s"# attributes: $attrs properties: B(1,2)", attrs, factory)
  }
  private def mine(input: FIMI, options: MiningOptions): Seq[String] = {
    val plan = SplitPlan.prepare(input, options)
    val out = scala.collection.mutable.ArrayBuffer[String]()
    plan.shards.foreach(shard => SplitPlan.mine(shard, plan.metadata, options, new Sink {
      def apply(h: Hypothesis): Unit = out += h.intent.toSeq.sorted.mkString(",")
      def close(): Unit = ()
    }))
    out.sorted
  }
  private def oracle(rows: Seq[Set[Int]], attrs: Int, support: Int): Seq[String] = {
    (1 until (1 << attrs)).flatMap { mask =>
      val intent = (0 until attrs).filter(a => (mask & (1 << a)) != 0).toSet
      val extent = rows.filter(intent.subsetOf)
      if (extent.size >= support && extent.reduce(_ intersect _) == intent)
        Some(intent.toSeq.sorted.mkString(",")) else None
    }.sorted
  }
  test("every three-row, three-attribute binary context matches a closure oracle without duplicates") {
    for (encoded <- 0 until 512) {
      val rows = (0 until 3).map { row =>
        (0 until 3).filter(a => (encoded & (1 << (row * 3 + a))) != 0).toSet
      }
      for (splits <- 1 to 3; support <- 1 to 3) {
        withClue(s"rows=$rows splits=$splits support=$support: ") {
          mine(data(rows, 3), MiningOptions(splits, support)) shouldEqual oracle(rows, 3, support)
        }
      }
    }
  }
  test("common root closure has one owner") {
    mine(data(Seq(Set(0, 1), Set(0, 1)), 2), MiningOptions(2, 1)) shouldEqual Seq("0,1")
  }
  test("payload and metadata survive Java serialization") {
    val plan = SplitPlan.prepare(data(Seq(Set(0), Set(1, 2)), 3), MiningOptions(2, 1))
    val bytes = new ByteArrayOutputStream
    val writer = new ObjectOutputStream(bytes)
    writer.writeObject((plan.metadata, plan.shards)); writer.close()
    val reader = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))
    val restored = reader.readObject().asInstanceOf[(SplitMetadata, Vector[SplitPayload])]
    reader.close()
    restored._1.order.toSeq shouldEqual plan.metadata.order.toSeq
    restored._2.map(_.rows.map(_.toSeq).toSeq) shouldEqual plan.shards.map(_.rows.map(_.toSeq).toSeq)
  }
  test("property filtering and restored attribute IDs match the original CLI algorithm") {
    val input = "# attributes: 4 properties: B(1,2)\n0 2 | 1\n0 2 3 | 1\n1 2 | 2\n2 3 | 2\n"
    val reference = new ByteArrayOutputStream
    JSM.generate(new ByteArrayInputStream(input.getBytes("UTF-8")), reference,
      "cbo", "dense", "noCounterExamples", 1.0, 1, 1)
    val loaded = FIMI.load(new ByteArrayInputStream(input.getBytes("UTF-8")), a => new ArrayInt(a))
    val options = MiningOptions(3, 1)
    val plan = SplitPlan.prepare(loaded, options)
    val actual = plan.shards.flatMap { shard =>
      var state = Option.empty[jsm4s.algorithm.CbOState]
      val results = Vector.newBuilder[String]
      do {
        val result = ShardRound.advance(shard, plan.metadata, options.copy(roundSteps = 2), state)
        state = Some(result.state)
        results ++= result.records
      } while (!state.get.done)
      results.result()
    }
    actual.sorted shouldEqual reference.toString("UTF-8").split("\n").drop(1).toVector.sorted
  }
  test("empty datasets and empty shards produce no hypotheses") {
    mine(data(Seq.empty, 3), MiningOptions(3, 1)) shouldBe empty
    mine(data(Seq(Set(2)), 3), MiningOptions(3, 1)) shouldEqual Seq("2")
  }
  test("invalid configuration is rejected before mining") {
    intercept[IllegalArgumentException](MiningOptions(0))
    intercept[IllegalArgumentException](MiningOptions(1, 0))
    intercept[IllegalArgumentException](MiningOptions(1, roundSteps = 0))
    intercept[IllegalArgumentException](MiningOptions(1, dataStructure = "sparse"))
    intercept[IllegalArgumentException](MiningOptions(1, strategy = "noop"))
    intercept[IllegalArgumentException](SplitPlan.prepare(data(Seq(Set(0)), 2), MiningOptions(3)))
  }
}
