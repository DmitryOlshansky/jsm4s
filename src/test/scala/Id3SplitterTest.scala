import jsm4s.attribute.{Id3Point, Id3Splitter}
import org.scalatest.{FlatSpec, Matchers}

class Id3SplitterTest extends FlatSpec with Matchers {

  class Fixture {
    def entry(v: Double, args: (String, Int)*) = Id3Point(v, args.toMap)

    val points = Array(
      entry(1.0, "+" -> 1),
      entry(3.0, "-" -> 1),
      entry(4.0, "+" -> 1),
      entry(5.0, "-" -> 2),
      entry(6.0, "+" -> 1),
      entry(7.0, "+" -> 1, "-" -> 1),
      entry(8.0, "-" -> 1)
    )
  }

  "Id3Splitter" should "calculate entropy for splits correctly" in new Fixture {
    val splitter = new Id3Splitter(points)
    splitter.entropyForRange(0, points.length)._1 shouldBe (0.9911 +- 0.001)
    splitter.entropyOfSplit(1, 0, points.length) shouldBe (0.8484 +- 0.001)
    splitter.entropyOfSplit(2, 0, points.length) shouldBe (0.9885 +- 0.001)
    splitter.entropyOfSplit(points.length-1, 0, points.length) shouldBe (0.8889 +- 0.001)
    // same but combine with lower-bound calculations
    splitter.entropyOfSplit(splitter.lowerBound(5.5), 0, points.length) shouldBe (0.9839 +- 0.001)
    splitter.entropyOfSplit(splitter.lowerBound(7.5), 0, points.length) shouldBe (0.8889 +- 0.001)
  }


}
