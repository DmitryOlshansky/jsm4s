package jsm4s.ds

import scala.collection.Iterable

/**
  * Created by olshanskiy on 7/13/17.
  */
trait IntentFactory {
  val attributes: Int

  def empty: FcaSet

  def full: FcaSet

  def values(x: Iterable[Int]): FcaSet
}
