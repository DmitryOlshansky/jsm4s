package jsm4s.ds

import scala.collection.Iterable

/**
  * Created by olshanskiy on 7/13/17.
  */
trait IntentFactory {
  val attributes: Int

  def emptyIntent: FcaSet

  def fullIntent: FcaSet

  def newIntent(x: Iterable[Int]): FcaSet
}
