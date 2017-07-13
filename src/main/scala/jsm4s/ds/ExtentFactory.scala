package jsm4s.ds

import scala.collection.Iterable

/**
  * Created by olshanskiy on 7/13/17.
  */
trait ExtentFactory {
  val objects: Int

  def emptyExtent: FcaSet

  def fullExtent: FcaSet

  def newExtent(x: Iterable[Int]): FcaSet
}
