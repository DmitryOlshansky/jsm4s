package jsm4s.attribute

class EnumAttribute(properties: Set[String], offset:Int) extends Attribute {
  private val mapping = properties.zipWithIndex.toMap

  override def apply(value: String) = Seq(offset + mapping(value))

  override def size: Int = mapping.size

  override def toString = s"$offset:${properties.mkString}"
}
