package jsm4s

import org.scalatest._

class TestBitSet extends FlatSpec with Matchers {
  "Bit set" should "work like integer set" in {
    val b1 = BitSet.full(7)
    val b2 = BitSet.empty.dup
    assert(b2.mkString(",") == "")
    b2 += 4
    b1 += 121
    b2 += 121
    val b3 = b1 & b2
    assert(b3.mkString(",") == "4,121")
  }
}
