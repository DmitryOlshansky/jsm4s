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

trait PropertyFactory {
  def tau: Property
  def empty: Property
  def encode(value: String): Property
  def decode(property: Property): String
}

object PropertyFactory {
  def apply(description: String) = {
    val composite = new Composite.Factory(description)
    if (composite.factories.size == 1) composite.factories.head
    else composite
  }
}