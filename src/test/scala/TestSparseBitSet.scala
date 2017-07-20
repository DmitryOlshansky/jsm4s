package jsm4s

import org.scalatest._
import jsm4s.ds.SparseBitSet

class TestSparseBitSet extends FlatSpec with Matchers {
  "SparseBitSet" should "work like integer set" in {
    val b1 = SparseBitSet.empty(700)
    val b2 = SparseBitSet.empty(700)
    b1.iterator.hasNext shouldBe false
    assert(b2.mkString(",") == "")
    b1 += 4
    b1 += 7
    b1 += 121
    assert(b1.contains(4))
    assert(b1.contains(7))
    assert(b1.contains(121))
    b2 += 4 
    b2 += 121
    assert(b2.contains(4))
    assert(b2.contains(121))
    val b3 = b1 & b2
    b3.mkString(",").split(",").map(_.toInt).sorted shouldEqual Array(4, 121)
    assert(b3 != b1)
    assert(b3 != b2)
    val b6 = SparseBitSet.full(65)
    val b7 = SparseBitSet.empty(65)
    b7 += 64
    (b6 & b7).toSeq.sorted shouldEqual Array(64)
    b7.until(64).toSeq.sorted shouldEqual Array()

    val b4 = SparseBitSet.full(7)
    val b5 = SparseBitSet.empty(7)
    b5 += 1
    b5 += 3
    (b4 & b5).toSeq.sorted shouldEqual Array(1, 3)
    b4.until(2).toSeq.sorted shouldEqual Array(0, 1)
    b5.until(2).toSeq.sorted shouldEqual Array(1)
    assert((b4.until(2) & b5.until(2)).mkString(" ") == "1")
    assert(b5.subsetOf(b4, 6))

    val b8 = SparseBitSet((0 until 1024).filter{ x=> x % 2 == 1 || x == 126 })
    val b9 = SparseBitSet((0 until 1024).filter{ x=> x % 2 == 0 || x== 123 })
    (b8 & b9).toSeq.sorted shouldEqual Array(123, 126)
    b8.subsetOf(b9, 128) shouldBe false
    (b8 & b9).subsetOf(b8, 128) shouldBe true
    (b8 & b9).subsetOf(b9, 128) shouldBe true
  }
}
