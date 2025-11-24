package jsm4s.property

case class OrdinalProperty(val value: Int) extends Property {
  override def &(prop: Property): OrdinalProperty = {
    if (value == prop.asInstanceOf[OrdinalProperty].value) this
    else OrdinalProperty.Empty
  }

  // we intern binary properties, so ref equality works
  override def equals(o: scala.Any): Boolean = 
    this.value == o.asInstanceOf[OrdinalProperty].value

  override def empty: Boolean = false

  override def tau: Boolean = false

  override def key = value
}

object OrdinalProperty {
    
  class Factory(values: Seq[String]) extends PropertyFactory {
    val order = values.sorted
    override def encode(value: String) = value match {
      case "?" => Tau
      case "*" => Empty
      case x if order.contains(x) => new OrdinalProperty(order.indexOf(x))
      case value => throw new PropertyException(s"Illegal value for ordinal property `$value`")
    }

    override def decode(prop: Property): String = prop match {
      case Tau => "?"
      case Empty => "*"
      case x: OrdinalProperty => order(x.value)
      case value => throw new PropertyException(s"Expected ordinal property but got `$value`")
    }

    override def tau: Property = Tau

    override def empty: Property = Empty
  }

  object Empty extends OrdinalProperty(-1) {
    override def empty = true
  }

  object Tau extends OrdinalProperty(-2) {
    override def &(prop: Property): OrdinalProperty = prop.asInstanceOf[OrdinalProperty]

    override def tau = true
  }
}



