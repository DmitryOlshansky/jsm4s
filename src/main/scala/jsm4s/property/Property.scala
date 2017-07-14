package jsm4s.property

trait Property {
  /// If this property is tau, that is undefined
  def tau: Boolean
  /// If this property is empty, which signals an intersection of
  def empty: Boolean
  /// Intersect this property with some other property, assumes same type of property
  def &(p:Property): Property
  /// String encoding of this property
  def toString: String
}

object Property {
  type Factory = String => Property
}
