package jsm4s.ds

import java.util

class PackedArray(var table: Array[Byte], var top: Int, var tsize: Int) extends FcaSet {
  override def contains(x: Int): Boolean = ???

  override def +=(x: Int): FcaSet = {
    val delta = x - top
    val mask = (1<<7) - 1
    val encode_length = (delta + (1<<7)) / (1<<7)
    if (tsize + encode_length > table.length) {
      val newSize = if (table.isEmpty) 4 else table.length * 2
      table = util.Arrays.copyOf(table, newSize)
    }
    var d = delta
    table(tsize) = (d & mask).toByte
    d >>= 7
    tsize += 1
    while(d > 0) {
      table(tsize) = (d & mask).toByte
      d >>= 7
      tsize += 1
    }
    table(tsize-1) = (table(tsize-1) | 0x80).toByte
    top = x
    this
  }

  override def &(set: FcaSet): FcaSet = ???

  override def &=(set: FcaSet): FcaSet = ???

  override def until(j: Int): FcaSet = ???

  override def dup: FcaSet = new PackedArray(util.Arrays.copyOf(table, table.length), top, tsize)

  override def ==(that: FcaSet): Boolean = util.Arrays.equals(table, that.asInstanceOf[PackedArray].table)

  override def equalUpTo(that: FcaSet, j: Int): Boolean = ???

  override def subsetOf(that: FcaSet, upTo: Int): Boolean = ???

  override def foreach[U](f: Int => U): Unit = {
    var i = 0
    var delta = 0
    var accum = 0
    var shift = 0
    while (i < tsize) {
      delta |= (table(i) & 0x7f) << shift
      shift += 7
      if (table(i) < 0) {
        accum += delta
        f(accum)
        delta = 0
        shift = 0
      }
      i += 1
    }
  }

  override def iterator: Iterator[Int] = new Iterator[Int] {
    var i = 0
    var delta = 0
    var accum = 0

    override def hasNext: Boolean = i < tsize

    override def next(): Int = {
      var shift = 0
      while(table(i) >= 0) {
        delta |= table(i) << shift
        shift += 7
        i += 1
      }
      delta |= (table(i) & 0x7f) << shift
      i += 1
      accum += delta
      accum
    }
  }
}

object PackedArray {
  val empty = new PackedArray(Array.emptyByteArray, 0, 0)

  def apply(seq: Iterable[Int]) = {
    val base = empty.dup
    seq.foreach(base += _)
    base
  }
}

trait PackedArrayExt extends ExtentFactory {
  override val emptyExtent = PackedArray.empty
  override val fullExtent = PackedArray(0.until(objects))

  override def newExtent(seq: Iterable[Int]) = PackedArray(seq)
}

