package jsm4s.ds

import java.util.Arrays

class SortedArray(var table: Array[Int], var tsize: Int) extends FcaSet with Iterable[Int] {

  override def contains(elem: Int): Boolean = {
    Arrays.binarySearch(table, 0, tsize, elem) >= 0
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = 0

    override def hasNext: Boolean = i != tsize

    override def next(): Int = {
      val item = table(i)
      i += 1
      item
    }
  }

  override def foreach[U](f: (Int) => U): Unit = {
    var i = 0
    while (i < tsize) {
      f(table(i))
      i += 1
    }
  }

  override def &(set: FcaSet): FcaSet = {
    val target = set.asInstanceOf[SortedArray]
    val result = Array.ofDim[Int](math.min(tsize, target.tsize))
    val rsize = UIntSetOps.intersect(table, tsize, target.table, target.tsize, result)
    new SortedArray(result, rsize)
  }

  override def &=(set: FcaSet): FcaSet = {
    val target = set.asInstanceOf[SortedArray]
    val result = Array.ofDim[Int](math.min(tsize, target.tsize))
    val rsize = UIntSetOps.intersect(table, tsize, target.table, target.tsize, result)
    table = result
    tsize = rsize
    this
  }

  override def until(j: Int): FcaSet = {
    new SortedArray(Arrays.copyOf(table, j), j)
  }

  override def +=(x: Int): FcaSet = {

    if (table.length == tsize) {
      val newSize = if (table.length == 0) 4 else table.length * 2
      table = Arrays.copyOf(table, newSize)
    }
    table(tsize) = x
    tsize += 1
    this
  }

  override def dup: FcaSet = new SortedArray(Arrays.copyOf(table, table.length), tsize)

  def ==(that: FcaSet): Boolean = {
    val target = that.asInstanceOf[SortedArray]
    for (i <- 0 until math.min(tsize, target.tsize)) {
      if (table(i) != target.table(i)) return false
    }
    true
  }

  override def subsetOf(that: FcaSet, k: Int): Boolean = {
    var i = 0
    var j = 0
    val rhs = that.asInstanceOf[SortedArray]
    while (i < tsize && j < rhs.tsize && table(i) < k) {
      if (table(i) == rhs.table(j)) {
        i += 1
        j += 1
      }
      else if(table(i) < rhs.table(j)) {
        i += 1
      }
      else if(table(i) > rhs.table(j)) {
        j += 1
      }
    }
    i == tsize || i == k
  }

  override def size: Int = tsize
}

object SortedArray {
  val empty = new SortedArray(Array[Int](), 0)
  def apply(seq: Iterable[Int]) = {
    val v = seq.toArray
    new SortedArray(v, v.length)
  }
}

trait ArrayExt extends ExtentFactory {
  override val emptyExtent = SortedArray.empty
  override val fullExtent = SortedArray(0.until(objects))

  override def newExtent(seq: Iterable[Int]) = SortedArray(seq)
}

trait ArrayInt extends IntentFactory {
  override val emptyIntent = SortedArray.empty
  override val fullIntent = SortedArray(0.until(attributes))

  override def newIntent(seq: Iterable[Int]) = SortedArray(seq)
}
