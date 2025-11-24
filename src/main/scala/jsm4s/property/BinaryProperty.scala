package jsm4s.property

import jsm4s.Utils.ensure

sealed trait BinaryProperty extends Property {
  def positive: Boolean

  def negative: Boolean

  override def &(prop: Property): BinaryProperty

  // we intern binary properties, so ref equality works
  override def equals(o: scala.Any): Boolean = this eq o.asInstanceOf[Property]
}

object BinaryProperty {

  case object Positive extends BinaryProperty {
    override def positive: Boolean = true

    override def negative: Boolean = false

    override def empty: Boolean = false

    override def tau: Boolean = false

    override def key = 1

    override def &(prop: Property): BinaryProperty =
      if (prop.asInstanceOf[BinaryProperty].positive) Positive else Empty

    override def toString: String = "1"
  }

  case object Negative extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = true

    override def empty: Boolean = false

    override def tau: Boolean = false

    override def key = 2

    override def &(prop: Property): BinaryProperty =
      if (prop.asInstanceOf[BinaryProperty].negative) Negative else Empty

    override def toString: String = "2"
  }

  case object Empty extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = false

    override def empty: Boolean = true

    override def tau: Boolean = false

    override def key = 0

    override def &(prop: Property): BinaryProperty = Empty

    override def toString: String = "0"
  }

  case object Tau extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = false

    override def empty: Boolean = false

    override def tau: Boolean = true

    override def key = 3

    override def &(prop: Property): BinaryProperty = prop.asInstanceOf[BinaryProperty]

    override def toString: String = "3"
  }

  class Factory(values: Seq[String]) extends PropertyFactory {
    val first = values.head
    val last = values.last
    ensure(values.size == 2, new PropertyException(s"Illegal number of values for binary property: ${values.size}"))

    override def encode(value: String) = value match {
      case x if x == first => Positive
      case x if x == last => Negative
      case "?" => Tau
      case "*" => Empty
      case value => throw new PropertyException(s"Illegal value for binary property `$value`")
    }

    override def decode(prop: Property): String = prop match {
      case Positive => first
      case Negative => last
      case Tau => "?"
      case Empty => "*"
    }

    override def tau: Property = Tau

    override def empty: Property = Empty

  }
}
