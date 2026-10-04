package jsm4s.algorithm

import java.io._
import jsm4s.ds._
import jsm4s.property.BinaryProperty
import org.scalatest.{FunSuite, Matchers}
import scala.collection.mutable.ArrayBuffer

class ResumableCbOSpec extends FunSuite with Matchers {
  private class Counters extends StatsCollector {
    var closures = 0L
    var failures = 0L
    def onClosure(): Unit = { closures += 1 }
    def onCanonicalTestFailure(): Unit = { failures += 1 }
    def printStats(): Unit = ()
  }
  private def context(rows: Seq[Seq[Int]], out: ArrayBuffer[String], stats: Counters,
                      firstAttribute: Int = 0): Context = {
    val int = new BitInt(3)
    val sink = new Sink {
      def apply(h: Hypothesis): Unit = { out += h.intent.mkString(",") }
      def close(): Unit = fail("advance must not close the caller's sink")
    }
    new Context(rows.map(int.values), rows.map(_ => BinaryProperty.Positive), 3, 1,
      stats, sink, new ArrayExt(rows.size), int, Strategies.noCounterExamples,
      NoSampling) { override def first: Int = firstAttribute }
  }
  private def roundTrip[A](state: A): A = {
    val bytes = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes)
    out.writeObject(state); out.close()
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray))
    try in.readObject().asInstanceOf[A] finally in.close()
  }

  private def contents(state: CbOState) =
    (state.attributes, state.objects, state.first, state.frames.map(f =>
      (f.extent.toVector, f.intent.toVector, f.nextAttribute, f.emitPending)))

  test("all FcaSet representations serialize their arrays and remain independently mutable") {
    val sets = Seq[FcaSet](new ArrayInt(66).values(Seq(0, 32, 64)),
      new BitInt(66).values(Seq(0, 32, 64)), new SparseBitInt(66).values(Seq(0, 32, 64)))
    sets.foreach { original =>
      val restored = roundTrip(original)
      restored.getClass shouldEqual original.getClass
      restored.toVector shouldEqual original.toVector
      restored += 65 // SortedArray appends in increasing order.
      original.contains(65) shouldBe false
      restored.contains(65) shouldBe true
    }
  }

  test("snapshots share read-only frames and sets while miners advance independently") {
    val rows = Seq(Seq(0, 1), Seq(1, 2), Seq(0, 2))
    val out = ArrayBuffer.empty[String]
    val miner = new ResumableCbO(context(rows, out, new Counters))
    val saved = miner.snapshot()
    val expected = contents(saved)
    val replayOut = ArrayBuffer.empty[String]
    val restored = new ResumableCbO(context(rows, replayOut, new Counters), Some(saved))
    (miner.snapshot().frames.head eq saved.frames.head) shouldBe true
    (restored.snapshot().frames.head eq saved.frames.head) shouldBe true
    miner.advance(1)
    val advanced = miner.snapshot().frames.head
    (advanced eq saved.frames.head) shouldBe false
    (advanced.extent eq saved.frames.head.extent) shouldBe true
    (advanced.intent eq saved.frames.head.intent) shouldBe true
    miner.advance(4)
    contents(saved) shouldEqual expected
    contents(restored.snapshot()) shouldEqual expected
    restored.run(false)
    contents(saved) shouldEqual expected
    miner.run(false)
    out.toVector shouldEqual replayOut.toVector
    val frame = saved.frames.head
    frame.extent shouldBe a [SortedArray]
    frame.intent shouldBe a [BitSet]
    intercept[IllegalArgumentException] {
      new ResumableCbO(context(rows, out, new Counters),
        Some(saved.copy(frames = Vector(frame.copy(intent = new ArrayInt(3).empty)))))
    }
  }

  test("pause and restore preserve recursive DFS output order, multiplicity and closure statistics") {
    for (encoded <- 0 until 512; budget <- Seq(1L, 7L, 100L)) {
      val rows = (0 until 3).map(r => (0 until 3).filter(a => (encoded & (1 << (3 * r + a))) != 0))
      val expected = ArrayBuffer.empty[String]
      val expectedStats = new Counters
      new CbO(context(rows, expected, expectedStats)).run(false)
      val actual = ArrayBuffer.empty[String]
      val actualStats = new Counters
      var engine = new ResumableCbO(context(rows, actual, actualStats))
      var closures = 0L
      var failures = 0L
      while (!engine.done) {
        val progress = engine.advance(budget)
        progress.steps should be <= budget
        closures += progress.closures
        failures += progress.canonicalFailures
        engine = new ResumableCbO(context(rows, actual, actualStats), Some(engine.snapshot()))
      }
      withClue(s"context=$encoded budget=$budget: ") {
        actual.toVector shouldEqual expected.toVector
        closures shouldEqual expectedStats.closures
        failures shouldEqual expectedStats.failures
        actualStats.closures shouldEqual expectedStats.closures
      }
    }
  }

  test("serialized snapshots are independent, replayable and preserve pending output") {
    val rows = Seq(Seq(0, 1), Seq(1, 2), Seq(0, 2))
    val before = ArrayBuffer.empty[String]
    val miner = new ResumableCbO(context(rows, before, new Counters))
    val initial = roundTrip(miner.snapshot())
    miner.advance(4)
    val saved = roundTrip(miner.snapshot())
    val after = ArrayBuffer.empty[String]
    val restored = new ResumableCbO(context(rows, after, new Counters), Some(saved))
    while (!restored.done) restored.advance(1)
    val sameSuffix = ArrayBuffer.empty[String]
    new ResumableCbO(context(rows, sameSuffix, new Counters), Some(saved)).run(false)
    after.toVector shouldEqual sameSuffix.toVector
    val uninterrupted = ArrayBuffer.empty[String]
    new ResumableCbO(context(rows, uninterrupted, new Counters), Some(initial)).run(false)
    (before ++ after).toVector shouldEqual uninterrupted.toVector
    contents(roundTrip(saved)) shouldEqual contents(saved)
    val completed = restored.snapshot()
    new ResumableCbO(context(rows, after, new Counters), Some(completed)).advance(100).steps shouldBe 0L
  }

  test("zero budget is a no-op and first attribute is preserved") {
    val out = ArrayBuffer.empty[String]
    val rows = Seq(Seq(0, 1), Seq(1, 2))
    val miner = new ResumableCbO(context(rows, out, new Counters, 1))
    val initial = miner.snapshot()
    miner.advance(0).steps shouldBe 0L
    contents(miner.snapshot()) shouldEqual contents(initial)
    out shouldBe empty
    intercept[IllegalArgumentException](miner.advance(-1))
    val expected = ArrayBuffer.empty[String]
    new CbO(context(rows, expected, new Counters, 1)).run(false)
    miner.run(false)
    out.toVector shouldEqual expected.toVector
    intercept[IllegalArgumentException] {
      new ResumableCbO(context(rows, out, new Counters, 0), Some(initial))
    }
  }
}
