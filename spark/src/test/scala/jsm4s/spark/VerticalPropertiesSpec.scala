package jsm4s.spark

import java.io._
import jsm4s.{FIMI, JSM}
import jsm4s.algorithm.{Hypothesis, Sink}
import jsm4s.ds.ArrayInt
import jsm4s.property.{PropertyException, PropertyFactory}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable.ArrayBuffer

class VerticalPropertiesSpec extends AnyFunSuite with Matchers {
  private def input(rows: Seq[Set[Int]], attrs: Int, values: Seq[String],
                    description: String = "B(1,2)"): FIMI = {
    val ints = new ArrayInt(attrs)
    val factory = PropertyFactory(description)
    FIMI(rows.map(r => ints.values(r.toSeq.sorted)).toIndexedSeq, values.map(factory.encode).toIndexedSeq,
      s"# attributes: $attrs properties: $description", attrs, factory)
  }
  private def roundTrip[A](value: A): A = {
    val bytes = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes)
    out.writeObject(value); out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))
    try in.readObject().asInstanceOf[A] finally in.close()
  }
  private case class Run(candidates: Vector[SupportedVerticalCandidate], rejected: Long)
  private def run(plan: VerticalPlan, options: MiningOptions, limit: Int, serialized: Boolean = false): Run = {
    var state = Option.empty[VerticalState]
    val out = Vector.newBuilder[SupportedVerticalCandidate]
    var rejected = 0L
    var rounds = 0
    do {
      val batch = VerticalRound.advance(plan.vertical, plan.metadata, options, limit, state)
      val saved = if (serialized) roundTrip(batch) else batch
      saved.candidates.size should be <= limit
      saved.progress.steps should be <= options.roundSteps
      out ++= saved.candidates
      rejected += saved.rejectedProperties
      state = Some(saved.state)
      rounds += 1
      assert(rounds < 200, "V did not finish")
    } while (!state.get.done)
    val result = out.result()
    result.map(_.candidate.id) shouldEqual result.indices.map(_.toLong).toVector
    state.get.nextCandidateId shouldBe result.size.toLong
    Run(result, rejected)
  }
  private def record(item: SupportedVerticalCandidate, plan: VerticalPlan): String =
    item.candidate.intent.map(plan.metadata.order).toSeq.sorted.mkString(" ") + " | " + item.properties

  private def combined(plan: VerticalPlan, options: MiningOptions, limit: Int,
                        serialized: Boolean = false): Vector[String] = {
    val verifier = new LocalVerticalVerifier(plan, options.support)
    val out = ArrayBuffer.empty[String]
    run(plan, options, limit, serialized).candidates.foreach { item =>
      // Both paths already have a valid property; the fast path needs no H call.
      if (!item.requiresHorizontalCheck || verifier.verify(item.candidate).accepted)
        out += record(item, plan)
    }
    val (_, factory) = FIMI.parseFimiHeader(plan.metadata.header)
    plan.horizontal.foreach { h =>
      SplitPlan.mine(h, plan.metadata, options, new Sink {
        def apply(hyp: Hypothesis): Unit =
          out += hyp.intent.mkString(" ") + " | " + factory.decode(hyp.props)
        def close(): Unit = ()
      })
    }
    out.toVector.sorted.toVector
  }
  private def reference(data: FIMI, options: MiningOptions): Vector[String] = {
    val body = data.header + "\n" + data.intents.zip(data.props).map { case (intent, prop) =>
      intent.mkString(" ") + " | " + data.factory.decode(prop) + "\n"
    }.mkString
    val out = new ByteArrayOutputStream
    JSM.generate(new ByteArrayInputStream(body.getBytes("UTF-8")), out, "cbo", "dense",
      options.strategy, 1.0, options.support, 1)
    out.toString("UTF-8").split("\n").drop(1).sorted.toVector
  }

  test("mixed binary H/V hypotheses match the original CLI for every small context and cut") {
    for (encoded <- 0 until 64; signs <- 0 until 8) {
      val rows = (0 until 3).map(r => (0 until 2)
        .filter(a => (encoded & (1 << (2 * r + a))) != 0).toSet)
      val data = input(rows, 2, (0 until 3).map(r => if ((signs & (1 << r)) != 0) "1" else "2"))
      for (strategy <- Seq("noCounterExamples", "votingMajority", "boundedVotingMajority:67",
        "boundedVotingMajority:100"); support <- 1 to 3) {
        val options = MiningOptions(2, support, strategy = strategy)
        val expected = reference(data, options)
        for (cut <- 0 to 2; (steps, limit) <- Seq((1L, 1), (7L, 3))) {
          val plan = VerticalPlan.prepare(data, options, cut)
          withClue(s"context=$encoded signs=$signs strategy=$strategy support=$support cut=$cut: ") {
            combined(plan, options.copy(roundSteps = steps), limit) shouldEqual expected
          }
        }
      }
    }
  }

  test("a property-rejected root does not prune valid descendants or consume candidate IDs") {
    val data = input(Seq(Set(0), Set(0, 1)), 2, Seq("1", "2"))
    val options = MiningOptions(1, 1, roundSteps = 1)
    val plan = VerticalPlan.prepare(data, options, 0)
    val first = VerticalRound.advance(plan.vertical, plan.metadata, options, 1)
    first.candidates shouldBe empty
    first.rejectedProperties shouldBe 1L
    first.state.done shouldBe false
    first.state.nextCandidateId shouldBe 0L
    val small = run(plan, options, 1, serialized = true)
    val large = run(plan, options.copy(roundSteps = 100), 100)
    small.rejected shouldBe 1L
    small.rejected shouldBe large.rejected
    small.candidates.map(record(_, plan)) shouldEqual Vector("0 1 | 2")
    small.candidates.map(record(_, plan)) shouldEqual large.candidates.map(record(_, plan))
  }

  test("property filtering runs even when the support bound permits immediate structural acceptance") {
    val data = input(Seq(Set(1), Set(1), Set(0, 1)), 2, Seq("1", "2", "1"))
    val options = MiningOptions(1, 1)
    val plan = VerticalPlan.prepare(data, options, 1)
    plan.vertical.maxPrefixSupport shouldBe 1
    run(plan, options, 1).candidates shouldBe empty
    val majority = run(plan, options.copy(strategy = "votingMajority"), 1).candidates
    majority should have size 1
    majority.head.support shouldBe 3
    majority.head.properties shouldBe "1"
    majority.head.requiresHorizontalCheck shouldBe false
    run(plan, options.copy(strategy = "boundedVotingMajority:67"), 1).candidates shouldBe empty
    run(plan, options.copy(strategy = "boundedVotingMajority:66"), 1).candidates.head.properties shouldBe "1"
  }

  test("ordinal, composite, partial empty, tau and empty properties preserve CLI semantics and serialize") {
    val rows = Seq(Set(0, 2), Set(0, 2), Set(1, 2), Set(2), Set(2, 3))
    val inputs = Seq(
      input(rows, 4, Seq("red", "red", "blue", "green", "red"), "O(red,green,blue)"),
      input(rows, 4, Seq("1 1", "1 2", "2 1", "2 1", "1 1"), "B(1,2)B(1,2)"),
      input(rows, 4, Seq("1 red", "1 blue", "2 blue", "2 blue", "1 red"), "B(1,2)O(red,blue)"),
      input(rows, 4, Seq("?", "1", "*", "2", "?")),
      input(rows, 4, Seq("?", "red", "*", "blue", "?"), "O(red,blue)"))
    for (data <- inputs; strategy <- Seq("noCounterExamples", "votingMajority",
      "boundedVotingMajority:0", "boundedVotingMajority:50", "boundedVotingMajority:100")) {
      val options = MiningOptions(3, 1, strategy = strategy, roundSteps = 2)
      val expected = reference(data, options)
      for (cut <- 0 to 4) {
        val plan = VerticalPlan.prepare(data, options, cut)
        withClue(s"header=${data.header} strategy=$strategy cut=$cut: ") {
          combined(plan, options, 1, serialized = true) shouldEqual expected
        }
      }
    }
    // Composite.empty means all components are empty, not just one component.
    val partial = input(Seq(Set(0), Set(0)), 1, Seq("1 1", "1 2"), "B(1,2)B(1,2)")
    val opts = MiningOptions(1, 1)
    val plan = VerticalPlan.prepare(partial, opts, 0)
    run(plan, opts, 1, serialized = true).candidates.head.properties shouldBe "1 *"
  }

  test("malformed properties fail before candidate output") {
    val options = MiningOptions(1, 1)
    val plan = VerticalPlan.prepare(input(Seq(Set(0)), 1, Seq("1")), options, 0)
    intercept[PropertyException] {
      VerticalRound.advance(plan.vertical.copy(properties = Array("invalid")), plan.metadata, options, 1)
    }
  }
}
