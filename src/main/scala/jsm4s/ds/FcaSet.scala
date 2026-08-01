package jsm4s.ds

import scala.collection.Iterable
import org.eclipse.collections.api.IntIterable
import org.eclipse.collections.api.iterator.IntIterator
/**
  * Created by olshanskiy on 7/13/17.
  */
/// A minimal integer set for FCA computations
trait FcaSet extends Iterable[Int] {
  def contains(x: Int): Boolean

  def +=(x: Int): FcaSet

  def &(set: FcaSet): FcaSet

  def &=(set: FcaSet): FcaSet

  def until(j: Int): FcaSet

  def dup: FcaSet

  def ==(that: FcaSet): Boolean

  def equalUpTo(that: FcaSet, j: Int): Boolean

  def equalWithMask(that: FcaSet, mask: FcaSet): Boolean = {
    (this & mask) == (that & mask)
  }

  def subsetOf(that: FcaSet, upTo: Int): Boolean

  override def mkString(start: String, sep: String, end: String): String = {
    val sb = new java.lang.StringBuilder()
    sb.append(start)
    val it = intIterator()
    var first = true
    while (it.hasNext) {
      val x = it.next()
      if (!first) sb.append(sep)
      sb.append(x)
      first = false
    }
    sb.append(end)
    sb.toString()
  }

  override def mkString(sep: String): String = mkString("", sep, "")

  def intIterator(): IntIterator

  def size: Int
}
