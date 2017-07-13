package jsm4s.property

trait Property {
  /// If this property is tau, that is undefined
  def tau: Boolean
  /// If this property is empty, which signals an intersection of
  def empty: Boolean
  /// String encoding of this property
  def mkString: String
}

object Property {
  type Factory = String => Property
}
