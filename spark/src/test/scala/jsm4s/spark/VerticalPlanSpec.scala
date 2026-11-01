package jsm4s.spark

import java.io._
import jsm4s.FIMI
import jsm4s.algorithm._
import jsm4s.ds.{ArrayInt, BitInt}
import jsm4s.property.BinaryProperty
import org.scalatest.{FunSuite, Matchers}

class VerticalPlanSpec extends FunSuite with Matchers {
  private val factory = new BinaryProperty.Factory(Seq("1", "2"))
  private def data(rows: Seq[Set[Int]], attrs: Int): FIMI = {
    val ints = new ArrayInt(attrs)
    FIMI(rows.map(r => ints.values(r.toSeq.sorted)), rows.map(_ => BinaryProperty.Positive),
      s"# attributes: $attrs properties: B(1,2)", attrs, factory)
  }
  private def sortedRows(input: FIMI, metadata: SplitMetadata): Seq[Set[Int]] = {
    val inverse = metadata.order.zipWithIndex.toMap
    input.intents.map(_.map(inverse).toSet)
  }
  private def candidate(id: Long, cut: Int, attrs: Int, intent: Iterable[Int]) =
    VerticalCandidate(id, cut, new ArrayInt(attrs).values(intent.toSeq.sorted))

  private def roundTrip[A](value: A): A = {
    val bytes = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes)
    out.writeObject(value); out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))
    try in.readObject().asInstanceOf[A] finally in.close()
  }

  test("cut keeps frequent trailing attributes with global IDs and preserves every row/property") {
    val input = data(Seq(Set(0, 2), Set(0, 2), Set(0, 1), Set.empty), 3)
      .copy(props = Seq(BinaryProperty.Positive, BinaryProperty.Negative,
        BinaryProperty.Positive, BinaryProperty.Negative))
    val plan = VerticalPlan.prepare(input, MiningOptions(3, 1), 2)
    plan.metadata.order.toVector shouldEqual Vector(1, 2, 0)
    plan.vertical.cut shouldBe 2
    plan.vertical.maxPrefixSupport shouldBe 2
    plan.vertical.rows.map(_.toVector).toVector shouldEqual
      Vector(Vector(2), Vector(2), Vector(2), Vector.empty)
    plan.vertical.properties.toVector shouldEqual input.props.map(factory.decode).toVector
    plan.horizontal.map(s => (s.first, s.last)) shouldEqual Vector((0, 0), (0, 1), (1, 2))
    plan.horizontal.head.rows shouldBe empty
    val ordered = sortedRows(input, plan.metadata)
    plan.horizontal.foreach { h =>
      h.rows.map(_.toSet).toVector shouldEqual
        ordered.filter(_.exists(a => a >= h.first && a < h.last)).toVector
    }
    // Cut construction must not mutate the caller's context.
    input.intents.map(_.toSet) shouldEqual Seq(Set(0, 2), Set(0, 2), Set(0, 1), Set.empty)
  }

  test("overlapping H support is not summed and local closure is not enough to reject") {
    val input = data(Seq(Set(0, 1, 2), Set(0, 1, 2), Set(2)), 3)
    val plan = VerticalPlan.prepare(input, MiningOptions(2, 1), 2)
    val result = new LocalVerticalVerifier(plan, 1).verify(candidate(7, 2, 3, Seq(2)))
    result.support shouldBe 3
    result.checks shouldBe empty // Support 3 exceeds both prefix supports (2).
    val rawChecks = plan.horizontal.map(h => new HorizontalCandidateVerifier(h, 3, 2)
      .check(candidate(7, 2, 3, Seq(2)), 3))
    rawChecks.map(_.matchingRows) shouldEqual Vector(2, 2)
    rawChecks.map(_.matchingRows).sum shouldBe 4
    result.omittedAttributes shouldBe empty
    result.accepted shouldBe true
    result.closure.toSet shouldEqual Set(2)
  }

  test("a prefix attribute common to every matching row rejects a suffix-closed candidate") {
    val input = data(Seq(Set(0, 1), Set(0, 1)), 2)
    val plan = VerticalPlan.prepare(input, MiningOptions(2, 1), 1)
    val result = new LocalVerticalVerifier(plan, 1).verify(candidate(9, 1, 2, Seq(1)))
    result.closedInProjection shouldBe true
    result.hasSupport shouldBe true
    result.support shouldEqual plan.vertical.maxPrefixSupport
    result.checks should not be empty // Equality must still run H verification.
    result.omittedAttributes.toSet shouldEqual Set(0)
    result.closure.toSet shouldEqual Set(0, 1)
    result.accepted shouldBe false
  }

  test("support bound does not bypass suffix closure or minimum support requirements") {
    val plan = VerticalPlan.prepare(data(Seq(Set(1, 2), Set(1, 2), Set(0, 2)), 3), MiningOptions(1, 1), 1)
    plan.vertical.maxPrefixSupport shouldBe 1
    val result = new LocalVerticalVerifier(plan, 1).verify(candidate(0, 1, 3, Seq(1)))
    result.support shouldBe 2
    result.checks shouldBe empty
    result.closedInProjection shouldBe false
    result.accepted shouldBe false
    val unsupported = new LocalVerticalVerifier(plan, 4).verify(candidate(1, 1, 3, Seq(2)))
    unsupported.checks shouldBe empty
    unsupported.hasSupport shouldBe false
    unsupported.accepted shouldBe false
  }

  test("every small context/cut matches full closure and combined H/V ownership without duplicates") {
    for (encoded <- 0 until 512) {
      val original = (0 until 3).map(r => (0 until 3)
        .filter(a => (encoded & (1 << (3 * r + a))) != 0).toSet)
      val input = data(original, 3)
      for (splits <- 1 to 3; cut <- 0 to 3) {
        val plan = VerticalPlan.prepare(input, MiningOptions(splits, 1), cut)
        val rows = sortedRows(input, plan.metadata)
        plan.vertical.maxPrefixSupport shouldEqual
          (0 until cut).map(a => rows.count(_.contains(a))).foldLeft(0)(math.max)
        val intents = (0 until (1 << (3 - cut))).map { mask =>
          (cut until 3).filter(a => (mask & (1 << (a - cut))) != 0).toSet
        }
        for (support <- 1 to 3) {
          val verifier = new LocalVerticalVerifier(plan, support)
          val results = intents.zipWithIndex.map { case (intent, id) =>
            val c = candidate(id, cut, 3, intent)
            val result = verifier.verify(c)
            val extent = rows.filter(intent.subsetOf)
            val closure = extent.foldLeft((0 until 3).toSet)(_ intersect _)
            withClue(s"context=$encoded splits=$splits cut=$cut support=$support intent=$intent: ") {
              result.support shouldEqual extent.size
              result.closure.toSet shouldEqual closure
              result.omittedAttributes.toSet shouldEqual closure.filter(_ < cut)
              result.closedInProjection shouldEqual (closure.filter(_ >= cut) == intent)
              result.accepted shouldEqual (intent.nonEmpty && extent.size >= support && closure == intent)
              c.intent.toSet shouldEqual intent
            }
            result
          }
          val horizontal = scala.collection.mutable.ArrayBuffer.empty[String]
          plan.horizontal.foreach { shard =>
            SplitPlan.mine(shard, plan.metadata, MiningOptions(splits, support), new Sink {
              def apply(h: Hypothesis): Unit = horizontal += h.intent.toSeq.sorted.mkString(",")
              def close(): Unit = ()
            })
          }
          val vertical = results.filter(_.accepted).map(_.closure.map(plan.metadata.order).toSeq.sorted.mkString(","))
          val expected = (1 until 8).flatMap { mask =>
            val intent = (0 until 3).filter(a => (mask & (1 << a)) != 0).toSet
            val extent = original.filter(intent.subsetOf)
            if (extent.size >= support && extent.reduce(_ intersect _) == intent)
              Some(intent.toSeq.sorted.mkString(",")) else None
          }
          (horizontal ++ vertical).sorted.toVector shouldEqual expected.sorted.toVector
        }
      }
    }
  }

  test("empty data, empty intents, insufficient support and nonclosed projections are not accepted") {
    val empty = VerticalPlan.prepare(data(Seq.empty, 3), MiningOptions(3, 1), 1)
    val result = new LocalVerticalVerifier(empty, 1).verify(candidate(0, 1, 3, Seq(2)))
    result.support shouldBe 0
    result.closure.toSet shouldEqual Set(0, 1, 2)
    result.accepted shouldBe false
    val plan = VerticalPlan.prepare(data(Seq(Set(0, 1)), 2), MiningOptions(1, 1), 0)
    val verifier = new LocalVerticalVerifier(plan, 1)
    verifier.verify(candidate(0, 0, 2, Seq.empty)).accepted shouldBe false
    verifier.verify(candidate(1, 0, 2, Seq(1))).closedInProjection shouldBe false
    new LocalVerticalVerifier(plan, 2).verify(candidate(2, 0, 2, Seq(0, 1))).hasSupport shouldBe false
  }

  test("plan, candidate and reply serialize and verification leaves paused H mining unchanged") {
    val plan = roundTrip(VerticalPlan.prepare(data(Seq(Set(0, 2), Set(1, 2), Set(0, 1, 2)), 3),
      MiningOptions(2, 1), 2))
    val c = roundTrip(candidate(42, 2, 3, Seq(2)))
    val miner = new ResumableCbO(SplitPlan.context(plan.horizontal.head, plan.metadata,
      MiningOptions(2, 1), NullSink))
    miner.advance(2)
    val saved = miner.snapshot()
    val verifier = new LocalVerticalVerifier(plan, 1)
    val first = verifier.verify(c)
    val replayed = roundTrip(verifier.verify(c))
    replayed.candidateId shouldBe 42L
    replayed.support shouldEqual first.support
    replayed.closure.toSet shouldEqual first.closure.toSet
    replayed.accepted shouldEqual first.accepted
    miner.snapshot() shouldEqual saved
  }

  test("invalid cuts, candidates, missing checkers and inconsistent support are rejected") {
    val input = data(Seq(Set(0, 1)), 2)
    val options = MiningOptions(1, 1)
    intercept[IllegalArgumentException](VerticalPlan.prepare(input, options, -1))
    intercept[IllegalArgumentException](VerticalPlan.prepare(input, options, 3))
    val plan = VerticalPlan.prepare(input, options, 1)
    val verifier = new LocalVerticalVerifier(plan, 1)
    intercept[IllegalArgumentException](verifier.verify(candidate(0, 0, 2, Seq(1))))
    intercept[IllegalArgumentException](verifier.verify(candidate(0, 1, 2, Seq(0))))
    intercept[IllegalArgumentException](verifier.verify(candidate(0, 1, 3, Seq(2))))
    intercept[IllegalArgumentException](new LocalVerticalVerifier(plan.copy(horizontal = Vector.empty), 1))
    intercept[IllegalArgumentException](new LocalVerticalVerifier(plan, 0))
    val check = new HorizontalCandidateVerifier(plan.horizontal.head, 2, 1)
    intercept[IllegalArgumentException](check.check(candidate(0, 1, 2, Seq(1)), 0))
  }

  test("dense candidates and cuts across multiple bitset words preserve the full closure") {
    val all = (0 until 70).toSet
    val plan = VerticalPlan.prepare(data(Seq(all, all), 70), MiningOptions(5, 2), 33)
    val intent = new BitInt(70).values(33 until 70)
    val result = new LocalVerticalVerifier(plan, 2).verify(VerticalCandidate(123L, 33, intent))
    result.support shouldBe 2
    result.closedInProjection shouldBe true
    result.closure.toSet shouldEqual all
    result.omittedAttributes.toSet shouldEqual (0 until 33).toSet
    result.accepted shouldBe false
    intent.toSet shouldEqual (33 until 70).toSet
  }
}
