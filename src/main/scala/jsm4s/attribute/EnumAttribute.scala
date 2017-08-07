package jsm4s.attribute

import scala.collection.SortedMap

class EnumAttribute(val values: SortedMap[String, Int], val offset:Int) extends Attribute {
  private val mapping = values.keys.zipWithIndex.toMap

  override def apply(value: String) = Seq(offset + mapping(value))

  override def size: Int = values.size

  override def toString = s"$offset:${values.mkString}"
}
