package jsm4s.ds

import java.util.Arrays
import org.eclipse.collections.api.IntIterable

class BitSet(val table: Array[Int], val length: Int) extends FcaSet with Serializable {

  override def contains(x: Int): Boolean = {
    if (x < length) (table(x / 32) & (1 << (x % 32))) != 0
    else false
  }

  override def until(j: Int): FcaSet = {
    val rem = if (j % 32 > 0) 1 else 0
    val mask = 1 << (j % 32)
    val newTable = Arrays.copyOf(table, table.length)
    for (i <- j / 32 + rem until table.length)
      newTable(i) = 0
    if (rem > 0) // mask away upper bits in the last word
      newTable(j / 32) &= (mask - 1)
    new BitSet(newTable, length)
  }

  override def &(set: FcaSet): FcaSet = {
    val bitset = set.asInstanceOf[BitSet]
    val result = Array.ofDim[Int](table.length)
    var i = 0
    while (i < table.length) {
      result(i) = table(i) & bitset.table(i)
      i += 1
    }
    new BitSet(result, length)
  }

  override def &=(set: FcaSet): FcaSet = {
    val bitset = set.asInstanceOf[BitSet]
    var i = 0
    while (i < table.length) {
      table(i) = table(i) & bitset.table(i)
      i += 1
    }
    this
  }

  override def +=(x: Int): FcaSet = {
    table(x / 32) |= 1 << (x % 32)
    this
  }

  override def dup: FcaSet = new BitSet(Arrays.copyOf(table, table.length), length)

  override def iterator: Iterator[Int] = {
    val len = length
    new Iterator[Int] {
      var i = 0

      override def hasNext: Boolean = {
        while (i < len && (table(i / 32) & (1 << (i % 32))) == 0) {
          i += 1
        }
        i != len
      }

      override def next(): Int = {
        val r = i
        i += 1
        r
      }
    }
  }

  override def ==(set: FcaSet): Boolean = {
    val bitset = set.asInstanceOf[BitSet]
    var i = 0
    while (i < table.length) {
      if (table(i) != bitset.table(i)) return false
      i += 1
    }
    true
  }

  override def equalWithMask(that: FcaSet, mask: FcaSet): Boolean = {
    val bitset = that.asInstanceOf[BitSet]
    val bitmask = mask.asInstanceOf[BitSet]
    var i = 0
    while (i < table.length) {
      if ((table(i) & bitmask.table(i)) != (bitset.table(i) & bitmask.table(i))) return false
      i += 1
    }
    true
  }

  override def equalUpTo(that: FcaSet, j: Int): Boolean = {
    val bitset = that.asInstanceOf[BitSet]
    val upTo = j / 32
    var i = 0
    while (i < upTo) {
      if (table(i) != bitset.table(i)) return false
      i += 1
    }
    val rem = j % 32
    if (rem > 0) {
      if ((table(i) & ((1<<rem)-1)) != (bitset.table(i) & ((1<<rem)-1))) return false
    }
    true
  }

  override def subsetOf(that: FcaSet, j: Int): Boolean = {
    val bitset = that.asInstanceOf[BitSet]
    val rem = j % 32
    for (i <- 0 until j / 32) {
      if ((table(i) & bitset.table(i)) != table(i)) return false
    }
    if (rem > 0) {
      val mask = (1 << rem) - 1
      val r = table(j / 32) & mask & bitset.table(j / 32)
      if (r != (table(j / 32) & mask)) return false
    }
    true
  }

  override def size = {
    var cnt = 0
    var i = 0
    while(i < table.size) {
      cnt += Integer.bitCount(table(i))
      i += 1
    }
    cnt
  }

}

object BitSet {
  def empty(size: Int):FcaSet = new BitSet(Array.ofDim[Int]((size + 31) / 32), size)

  def full(size: Int):FcaSet = {
    val words = size / 32
    val rem = if (size % 32 > 0) 1 else 0
    val mask = 1 << (size % 32)
    val arr = Array.ofDim[Int](words + rem)
    Arrays.fill(arr, -1)
    if (rem > 0) // mask away upper bits in the last word
      arr(arr.length - 1) &= (mask - 1)
    new BitSet(arr, size)
  }

  def apply(seq: Iterable[Int], length: Int):FcaSet = {
    val bs = new BitSet(Array.ofDim[Int]((length + 31) / 32), length)
    for (s <- seq.iterator) bs += s
    bs
  }

  def apply(seq: IntIterable, length: Int):FcaSet = {
    val bs = new BitSet(Array.ofDim[Int]((length + 31) / 32), length)
    val iter = seq.intIterator()
    while (iter.hasNext) {
      bs += iter.next()
    }
    bs
  }
}

class BitExt(val objects: Int) extends ExtentFactory {
  val empty = BitSet.empty(objects)
  val full = BitSet.full(objects)

  override def values(seq: Iterable[Int]) = BitSet(seq, objects)
}

class BitInt(val attributes: Int) extends IntentFactory {
  val empty = BitSet.empty(attributes)
  val full = BitSet.full(attributes)

  override def values(seq: Iterable[Int]) = BitSet(seq, attributes)
}
