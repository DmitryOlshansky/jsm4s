package jsm4s.attribute

import scala.collection.SortedMap
import scala.util.Try

trait Attribute {
  def apply(value: String): Seq[Int]
  def size: Int
}

object Attribute {

  private def isNumeric(value: String) = Try(value.toDouble).isSuccess

  def apply(values: Map[String, Map[String, Int]], offset: Int): Attribute = {
    if (values.keys.forall{ key => isNumeric(key)} && values.size > 10){
      new Id3Atribute(values, offset)
    }
    else new EnumAttribute(values.keys.toSet, offset)
  }
}