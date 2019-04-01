package jsm4s.property

case object NullProperty extends Property {
  override def tau: Boolean = false

  override def empty: Boolean = true

  override def &(p: Property): Property = NullProperty

  override def toString: String = "0"
}
