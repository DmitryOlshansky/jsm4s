package jsm4s.spark

import java.io._
import jsm4s.FIMI
import jsm4s.algorithm._
import jsm4s.ds.{ArrayExt, ArrayInt, BitInt, FcaSet}
import jsm4s.property.BinaryProperty
import org.scalatest.{FunSuite, Matchers}
import scala.collection.mutable.ArrayBuffer

class VerticalRoundSpec extends FunSuite with Matchers {
  private val factory = new BinaryProperty.Factory(Seq("1", "2"))
  private def data(rows: Seq[Set[Int]], attrs: Int): FIMI = {
    val ints = new ArrayInt(attrs)
    FIMI(rows.map(r => ints.values(r.toSeq.sorted)), rows.map(_ => BinaryProperty.Positive),
      s"# attributes: $attrs properties: B(1,2)", attrs, factory)
  }
  private def signature(c: SupportedVerticalCandidate) =
    (c.candidate.id, c.candidate.cut, c.candidate.intent.toVector, c.support, c.properties, c.requiresHorizontalCheck)
  private def stateSignature(s: VerticalState) =
    (s.nextCandidateId, s.minSupport, s.strategy, s.cbo.attributes, s.cbo.objects, s.cbo.first,
      s.cbo.frames.map(f => (f.extent.toVector, f.intent.toVector, f.nextAttribute, f.emitPending)))

  private def roundTrip[A](value: A): A = {
    val bytes = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes)
    out.writeObject(value); out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))
    try in.readObject().asInstanceOf[A] finally in.close()
  }

  private def collect(plan: VerticalPlan, options: MiningOptions, limit: Int,
                       serialize: Boolean = false): Vector[SupportedVerticalCandidate] = {
    val out = Vector.newBuilder[SupportedVerticalCandidate]
    var state = Option.empty[VerticalState]
    var rounds = 0
    do {
      val batch = VerticalRound.advance(plan.vertical, plan.metadata, options, limit, state)
      batch.progress.steps should be <= options.roundSteps
      batch.candidates.size should be <= limit
      batch.progress.done shouldEqual batch.state.done
      out ++= batch.candidates
      state = Some(if (serialize) roundTrip(batch.state) else batch.state)
      rounds += 1
      assert(rounds < 1000, "V failed to finish")
    } while (!state.get.done)
    val result = out.result()
    result.map(_.candidate.id) shouldEqual result.indices.map(_.toLong).toVector
    state.get.nextCandidateId shouldEqual result.size.toLong
    val finished = VerticalRound.advance(plan.vertical, plan.metadata, options, limit, state)
    finished.candidates shouldBe empty
    finished.progress.steps shouldBe 0L
    stateSignature(finished.state) shouldEqual stateSignature(state.get)
    result
  }

  // Independent recursive traversal of the projected context for exact DFS ordering.
  private def reference(plan: VerticalPlan, support: Int): Vector[(Vector[Int], Int)] = {
    val ints = new BitInt(plan.metadata.attributes)
    val rows = plan.vertical.rows.map(r => ints.values(r.toSeq)).toVector
    val out = Vector.newBuilder[(Vector[Int], Int)]
    val context = new Context(rows, Seq.empty, plan.metadata.attributes, support,
      new NullCollector, NullSink, new ArrayExt(rows.size), ints,
      Strategies.noCounterExamples, NoSampling) {
      override def first: Int = plan.vertical.cut
    }
    new CbO(context) {
      override def output(extent: FcaSet, intent: FcaSet): Unit = {
        if (extent.size >= support && intent.nonEmpty) out += ((intent.toVector, extent.size))
      }
    }.run(false)
    out.result()
  }

  test("all small projections retain recursive DFS order and exact support across both batch limits") {
    for (encoded <- 0 until 512; cut <- 0 to 3; support <- 1 to 3) {
      val original = (0 until 3).map(r => (0 until 3)
        .filter(a => (encoded & (1 << (3 * r + a))) != 0).toSet)
      val options = MiningOptions(2, support)
      val plan = VerticalPlan.prepare(data(original, 3), options, cut)
      val expected = reference(plan, support)
      for ((steps, limit) <- Seq((1L, 1), (7L, 2), (100L, 100))) {
        val result = collect(plan, options.copy(roundSteps = steps), limit)
        withClue(s"context=$encoded cut=$cut support=$support steps=$steps limit=$limit: ") {
          result.map(c => (c.candidate.intent.toVector, c.support)) shouldEqual expected
          val verifier = new LocalVerticalVerifier(plan, support)
          result.foreach { item =>
            item.candidate.cut shouldBe cut
            item.candidate.intent.forall(_ >= cut) shouldBe true
            val checked = verifier.verify(item.candidate)
            checked.closedInProjection shouldBe true
            checked.hasSupport shouldBe true
            checked.support shouldBe item.support
            item.requiresHorizontalCheck shouldEqual
              (cut != 0 && item.support <= plan.vertical.maxPrefixSupport)
            if (!item.requiresHorizontalCheck) {
              checked.accepted shouldBe true
              checked.checks shouldBe empty
            }
          }
        }
      }
    }
  }

  test("serialized continuation and replay preserve IDs, candidates, counters and prior state") {
    val options = MiningOptions(2, 1, roundSteps = 4)
    val plan = VerticalPlan.prepare(data(Seq(Set(0, 1), Set(1, 2), Set(0, 2)), 3), options, 0)
    val first = VerticalRound.advance(plan.vertical, plan.metadata, options, 1)
    val saved = stateSignature(first.state)
    val next = VerticalRound.advance(plan.vertical, plan.metadata, options, 1, Some(first.state))
    val replay = roundTrip(VerticalRound.advance(plan.vertical, plan.metadata, options, 1,
      Some(roundTrip(first.state))))
    next.candidates.map(signature) shouldEqual replay.candidates.map(signature)
    next.progress shouldEqual replay.progress
    next.rejectedProperties shouldEqual replay.rejectedProperties
    stateSignature(next.state) shouldEqual stateSignature(replay.state)
    stateSignature(first.state) shouldEqual saved
    collect(plan, options, 1, serialize = true).map(signature) shouldEqual
      collect(plan, options.copy(roundSteps = 100), 100).map(signature)
  }

  test("candidate backpressure pauses emission and empty step-limited batches are not completion") {
    val options = MiningOptions(1, 1, roundSteps = 100)
    val plan = VerticalPlan.prepare(data(Seq(Set(0), Set(1)), 2), options, 0)
    val paused = VerticalRound.advance(plan.vertical, plan.metadata, options, 0)
    paused.progress.steps shouldBe 0L
    paused.candidates shouldBe empty
    paused.state.nextCandidateId shouldBe 0L
    paused.state.done shouldBe false
    val idleBatch = VerticalRound.advance(plan.vertical, plan.metadata, options.copy(roundSteps = 1),
      1, Some(paused.state))
    idleBatch.progress.steps shouldBe 1L // Empty root intent consumes an emission step.
    idleBatch.candidates shouldBe empty
    idleBatch.state.done shouldBe false
    val batch = VerticalRound.advance(plan.vertical, plan.metadata, options, 1, Some(idleBatch.state))
    batch.candidates should have size 1
    batch.candidates.head.candidate.id shouldBe 0L
    batch.state.cbo.frames.last.emitPending shouldBe false
    val stopped = VerticalRound.advance(plan.vertical, plan.metadata, options, 0, Some(batch.state))
    stopped.progress.steps shouldBe 0L
    stateSignature(stopped.state) shouldEqual stateSignature(batch.state)
  }

  test("property filtering removes conflicts and duplicate rows retain their votes and support") {
    val input = data(Seq(Set(0, 1), Set(0, 1), Set(0, 1), Set.empty), 2)
      .copy(props = Seq(BinaryProperty.Positive, BinaryProperty.Negative,
        BinaryProperty.Positive, BinaryProperty.Positive))
    val options = MiningOptions(1, 2)
    val plan = VerticalPlan.prepare(input, options, 1)
    collect(plan, options, 1) shouldBe empty
    val result = collect(plan, options.copy(strategy = "votingMajority"), 1)
    result.map(c => (c.candidate.intent.toVector, c.support, c.properties)) shouldEqual Vector((Vector(1), 3, "1"))
    new LocalVerticalVerifier(plan, 2).verify(result.head.candidate).accepted shouldBe false
  }

  test("H output plus verified V batches equals unsplit CbO without deduplicating") {
    val rows = Seq(Set(0, 1, 2), Set(0, 2), Set(1, 2), Set(2), Set(2, 3))
    val input = data(rows, 4)
    val ints = new BitInt(4)
    val expected = ArrayBuffer.empty[String]
    val sink = new Sink {
      def apply(h: Hypothesis): Unit = expected += h.intent.toSeq.sorted.mkString(",")
      def close(): Unit = ()
    }
    new CbO(new Context(input.intents.map(r => ints.values(r)), input.props, 4, 1,
      new NullCollector, sink, new ArrayExt(rows.size), ints, Strategies.noCounterExamples, NoSampling)).run(false)
    for (cut <- 0 to 4; limit <- Seq(1, 3)) {
      val options = MiningOptions(3, 1, roundSteps = 2)
      val plan = VerticalPlan.prepare(input, options, cut)
      val actual = ArrayBuffer.empty[String]
      val verifier = new LocalVerticalVerifier(plan, 1)
      collect(plan, options, limit).foreach { item =>
        if (verifier.verify(item.candidate).accepted)
          actual += item.candidate.intent.map(plan.metadata.order).toSeq.sorted.mkString(",")
      }
      plan.horizontal.foreach(h => SplitPlan.mine(h, plan.metadata, options, new Sink {
        def apply(hyp: Hypothesis): Unit = actual += hyp.intent.toSeq.sorted.mkString(",")
        def close(): Unit = ()
      }))
      actual.sorted.toVector shouldEqual expected.sorted.toVector
    }
  }

  test("empty data, no suffix, and insufficient support terminate without candidates") {
    val options = MiningOptions(1, 2, roundSteps = 1)
    for ((rows, cut) <- Seq((Seq.empty[Set[Int]], 0), (Seq(Set(0, 1)), 0),
      (Seq(Set(0, 1), Set(0, 1)), 2))) {
      collect(VerticalPlan.prepare(data(rows, 2), options, cut), options, 1) shouldBe empty
    }
  }

  test("invalid limits, incompatible state, malformed projection and ID overflow fail explicitly") {
    val options = MiningOptions(1, 1)
    val plan = VerticalPlan.prepare(data(Seq(Set(0, 1)), 2), options, 1)
    val initial = VerticalRound.advance(plan.vertical, plan.metadata, options, 0).state
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical, plan.metadata, options, -1))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical, plan.metadata,
      options.copy(support = 2), 1, Some(initial)))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical, plan.metadata,
      options.copy(strategy = "votingMajority"), 1, Some(initial)))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical.copy(cut = 0), plan.metadata,
      options, 1, Some(initial)))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical, plan.metadata, options,
      1, Some(initial.copy(nextCandidateId = -1))))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical.copy(rows = Array(Array(0))),
      plan.metadata, options, 1))
    intercept[IllegalArgumentException](VerticalRound.advance(plan.vertical.copy(properties = Array.empty[String]),
      plan.metadata, options, 1))
    intercept[ArithmeticException](VerticalRound.advance(plan.vertical, plan.metadata, options,
      1, Some(initial.copy(nextCandidateId = Long.MaxValue))))
  }
}
