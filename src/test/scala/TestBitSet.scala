package jsm4s

import org.scalatest._
import jsm4s.ds.BitSet

class TestBitSet extends FlatSpec with Matchers {
  "Bit set" should "work like integer set" in {
    val b1 = BitSet.empty(700)
    val b2 = BitSet.empty(700)
    assert(b2.mkString(",") == "")
    b1 += 4
    b1 += 7
    b2 += 4
    b1 += 121
    b2 += 121
    val b3 = b1 & b2
    assert(b3.mkString(",") == "4,121")
    assert(b3 != b1)
    assert(b3 != b2)
    val b6 = BitSet.full(65)
    val b7 = BitSet.empty(65)
    b7 += 64
    assert((b6 & b7).mkString(" ") == "64")
    assert(b7.until(64).mkString(" ") == "")
    val b4 = BitSet.full(7)
    val b5 = BitSet.empty(7)
    b5 += 1
    b5 += 3
    assert(b5.subsetOf(b4, 6))
    assert((b4 & b5).mkString(" ") == "1 3")
    assert((b4.until(2) & b5.until(2)).mkString(" ") == "1")

  }
}

