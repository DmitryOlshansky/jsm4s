package jsm4s.attribute

import jsm4s.JsmException

import scala.collection.SortedMap

class EnumEncoder(val values: SortedMap[String, Int], val offset:Int) extends Encoder {
  private val mapping = values.keys.zipWithIndex.toMap

  override def apply(v: Any) = v match {
    case value: String => Seq(offset + mapping(value))
    case _ => throw JsmException(s"Not supported type ${v.getClass} for EnumEncoder")
  }

  override def size: Int = values.size

  override def toString = s"$offset:${values.mkString}"
}
