package jsm4s.ds

import java.util.Arrays.copyOf

class SparseBitSet(var table: Array[Long], var len: Int) extends FcaSet {

  private def extend(capacity: Int) = {
    if (capacity > table.length) {
      table = copyOf(table, capacity)
    }
    len = capacity
  }

  override def contains(x: Int): Boolean = {
    var i = 0
    val targetIndex = x / 32
    while (i < len) {
      val index = table(i) >>> 32
      if (index == targetIndex)
        return (table(i) & (1L << (x % 32))) != 0
      i += 1
    }
    false
  }

  override def +=(x: Int): FcaSet = {
    var i = 0
    val targetIndex = x / 32
    while (i < len) {
      val index = table(i) >>> 32
      if (index == targetIndex) {
        table(i) |= 1L << (x % 32)
        return this
      }
      else if (index > targetIndex) {
        extend(len + 1)
        Array.copy(table, i, table, i + 1, len - i)
        table(i) = targetIndex.toLong << 32 | (1L << (x % 32))
        return this
      }
      i += 1
    }
    extend(len + 1)
    table(len - 1) = targetIndex.toLong << 32 | (1L << (x % 32))
    this
  }

  override def &(set: FcaSet): FcaSet = {
    val that = set.asInstanceOf[SparseBitSet]
    val result = Array.ofDim[Long](Math.min(len, that.len))
    var i = 0
    var j = 0
    var k = 0
    while (i < len && j < that.len) {
      val aIndex = table(i) >>> 32
      val bIndex = that.table(j) >>> 32
      if (aIndex < bIndex) {
        i += 1
      }
      else if (aIndex > bIndex) {
        j += 1
      }
      else {
        val intersection = table(i) & that.table(j)
        if ((intersection & ((1L << 32) - 1)) != 0) {
          result(k) = intersection
          k += 1
        }
        i += 1
        j += 1
      }
    }
    new SparseBitSet(result, k)
  }

  override def &=(set: FcaSet): FcaSet = {
    val that = set.asInstanceOf[SparseBitSet]
    var i = 0
    var j = 0
    var k = 0
    while (i < len && j < that.len) {
      val aIndex = table(i) >>> 32
      val bIndex = that.table(j) >>> 32
      if (aIndex < bIndex) {
        i += 1
      }
      else if (aIndex > bIndex) {
        j += 1
      }
      else {
        val intersection = table(i) & that.table(j)
        if ((intersection & ((1L << 32) - 1)) != 0) {
          table(k) = intersection
          k += 1
        }
        i += 1
        j += 1
      }
    }
    len = k
    this
  }

  override def until(j: Int): FcaSet = {
    val targetIndex = j / 32
    val result = Array.ofDim[Long](len)
    var i = 0
    var k = 0
    while (i < len) {
      val index = table(i) >>> 32
      if (index > targetIndex)
        return new SparseBitSet(result, k)
      else if (index == targetIndex) {
        val res = (table(i) & ~((1L << 32) - 1)) | (table(i) & ((1L << (j % 32)) - 1))
        if ((res & ((1L<<32)-1)) != 0){
          result(k) = res
          k += 1
        }
      }
      else {
        result(k) = table(i)
        k += 1
      }
      i += 1
    }
    new SparseBitSet(result, k)
  }

  override def dup: FcaSet = new SparseBitSet(copyOf(table, len), len)

  override def ==(that: FcaSet): Boolean = {
    val lhs = this
    val rhs = that.asInstanceOf[SparseBitSet]
    if (len != rhs.len) false
    else {
      var i = 0
      while(i < len) {
        if (lhs.table(i) != rhs.table(i)) return false
        i += 1
      }
      true
    }
  }

  override def equalUpTo(that: FcaSet, j: Int): Boolean = ???

  override def subsetOf(that: FcaSet, y: Int): Boolean = {
    val lhs = until(y).asInstanceOf[SparseBitSet]
    val rhs = that.until(y).asInstanceOf[SparseBitSet]
    var i = 0
    var j = 0
    val yIndex = y / 32
    while (i < lhs.len && j < rhs.len) {
      val aIndex = lhs.table(i)>>>32
      val bIndex = rhs.table(j)>>>32
      if (aIndex < bIndex) {
        if (aIndex == yIndex) return true
        i += 1
      }
      else if(aIndex > bIndex) {
        if (bIndex == yIndex) return false
        j += 1
      }
      else {
        val lMask = lhs.table(i) & ((1L<<32)-1)
        val rMask = rhs.table(j) & ((1L<<32)-1)
        if (aIndex == yIndex) {
          val mask = (1L<<(y % 32)) - 1
          if(((lMask | rMask) & mask) != (rMask & mask)) return false
          return true
        }
        else if((lMask | rMask) != rMask) return false
        i += 1
        j += 1
      }
    }
    i == lhs.len
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = 0
    var j = 0

    override def hasNext: Boolean = {
      while (i != len) {
        while (j < 32 && (table(i) & (1L << j)) == 0) {
          j += 1
        }
        if (j == 32){
          i += 1
          j = 0
        }
        else return true
      }
      false
    }

    override def next(): Int = {
      val ret = (table(i) >>> 32).toInt * 32 + j
      j += 1
      if (j == 32) {
        j = 0
        i += 1
      }
      ret
    }
  }

  override def size = {
    var s = 0
    var i = 0
    while (i < len) {
      s += java.lang.Long.bitCount(table(i) & ((1L << 32) - 1))
      i += 1
    }
    s
  }
}

object SparseBitSet {

  def empty(capacity: Int) = new SparseBitSet(Array.emptyLongArray, 0)

  def full(capacity: Int) = SparseBitSet(0 until capacity)

  def apply(seq: Iterable[Int]) = {
    val set = SparseBitSet.empty(0)
    seq.foreach{ e => set += e }
    set
  }
}


class SparseBitExt(val objects: Int) extends ExtentFactory {
  override val empty = SparseBitSet.empty(objects)
  override val full = SparseBitSet.full(objects)

  override def values(seq: Iterable[Int]) = SparseBitSet(seq)
}

class SparseBitInt(val attributes: Int) extends IntentFactory {
  override val empty = SparseBitSet.empty(attributes)
  override val full = SparseBitSet.full(attributes)

  override def values(seq: Iterable[Int]) = SparseBitSet(seq)
}
