package jsm4s.ds

import scala.collection.immutable

class TreeSet(var set: immutable.TreeSet[Int]) extends FcaSet {
  def this(iterable: Iterable[Int]) = {
    this(immutable.TreeSet[Int]())
    for (x <- iterable) this += x
  }

  override def contains(x: Int): Boolean = set.contains(x)

  override def ==(that: FcaSet): Boolean = {
    val set = that.asInstanceOf[TreeSet]
    this.set == set.set
  }

  override def until(j: Int): FcaSet = {
    new TreeSet(set.until(j))
  }

  override def &(that: FcaSet): FcaSet = {
    val set = that.asInstanceOf[TreeSet]
    new TreeSet(this.set & set.set)
  }

  override def +=(x: Int): FcaSet = {
    set += x
    this
  }

  override def dup: FcaSet = new TreeSet(set)

  override def iterator: Iterator[Int] = set.iterator

  override def subsetOf(that: FcaSet, j: Int): Boolean = {
    val thatSet = that.asInstanceOf[TreeSet]
    set.until(j).subsetOf(thatSet.set.until(j))
  }

  override def size = set.size
}

object TreeSet {
  def empty = new TreeSet(immutable.TreeSet[Int]())

  def full(size: Int) = new TreeSet(immutable.TreeSet[Int](0 until (size): _*))
}

trait TreeExt extends ExtentFactory {
  val emptyExtent = TreeSet.empty
  val fullExtent = TreeSet.full(objects)
}

trait TreeInt extends IntentFactory {
  val emptyIntent = TreeSet.empty
  val fullIntent = TreeSet.full(attributes)
}