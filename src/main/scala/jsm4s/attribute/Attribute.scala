package jsm4s.attribute

import scala.collection.SortedMap
import scala.util.Try

trait Attribute {
  def apply(value: String): Seq[Int]
  def size: Int
}

object Attribute {

  private def isNumeric(value: String) = Try(value.toDouble).isSuccess

  def factory(values: SortedMap[String, Int], properties: SortedMap[String, Set[String]], offset: Int): Attribute = {
    if (values.keys.forall{ key => isNumeric(key)} && values.size > 10){
      new ClusteringNumericAttribute(values, properties, offset)
    }
    else new EnumAttribute(values, offset)
  }
}