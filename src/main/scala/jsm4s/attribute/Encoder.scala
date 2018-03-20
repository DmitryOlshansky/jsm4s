package jsm4s.attribute

import scala.collection.SortedMap
import scala.util.Try

trait Encoder {
  def apply(value: Any): Seq[Int]
  def size: Int
}

object Encoder {

  private def isNumeric(value: String) = Try(value.toDouble).isSuccess

  def factory(values: SortedMap[String, Int], properties: SortedMap[String, Set[String]], offset: Int): Encoder = {
    if (values.keys.forall{ key => isNumeric(key)} && values.size > 10){
      new ClusteringNumeric(values, properties, offset)
    }
    else new EnumEncoder(values, offset)
  }
}