import jsm4s.ds.PackedArray
import org.scalatest._

import scala.collection.mutable

class TestPackedArray extends FlatSpec with Matchers {
  "PackedArray" should "support minimum operations" in {
    val set = PackedArray.empty.dup
    set += 4
    set += 7
    set += 10000
    set += 10001
    val result = mutable.Buffer[Int]()
    set.foreach(result += _)
    assert(result == Seq(4, 7, 10000, 10001))
    assert(set.mkString(",") == "4,7,10000,10001")

    val set2 = PackedArray(Seq(1,0x80, 0x81))
    assert(set2.mkString(",") == "1,128,129")

    val set3 = PackedArray.empty.dup
    set3 += 0
    set3 += 128
    assert(set3.mkString(",") == "0,128")
  }

}

