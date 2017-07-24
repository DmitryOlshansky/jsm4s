package jsm4s.ds

import scala.collection.Iterable

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

  def equalWithMask(that: FcaSet, mask: FcaSet): Boolean = {
    (this & mask) == (that & mask)
  }

  def subsetOf(that: FcaSet, upTo: Int): Boolean

  def size: Int
}
