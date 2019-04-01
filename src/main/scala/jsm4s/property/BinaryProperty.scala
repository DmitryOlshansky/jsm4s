package jsm4s.property

import scala.collection.SortedSet

trait BinaryProperty extends Property {
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

    override def &(prop: Property): BinaryProperty =
      if (prop.asInstanceOf[BinaryProperty].positive) Positive else Empty

    override def toString: String = "1"
  }

  case object Negative extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = true

    override def empty: Boolean = false

    override def tau: Boolean = false

    override def &(prop: Property): BinaryProperty =
      if (prop.asInstanceOf[BinaryProperty].negative) Negative else Empty

    override def toString: String = "2"
  }

  case object Empty extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = false

    override def empty: Boolean = true

    override def tau: Boolean = false

    override def &(prop: Property): BinaryProperty = Empty

    override def toString: String = "0"
  }

  case object Tau extends BinaryProperty {
    override def positive: Boolean = false

    override def negative: Boolean = false

    override def empty: Boolean = false

    override def tau: Boolean = true

    override def &(prop: Property): BinaryProperty = prop.asInstanceOf[BinaryProperty]

    override def toString: String = "3"
  }


  def factory(values: SortedSet[String]):Property.Factory = {
    if (values.size != 2) throw  new PropertyException(s"Illegal number of values for binary property `${values.size}")
    val first = values.head
    (x) => {
      if (x == first) Positive
      else Negative
    }
  }

  def loader:Property.Factory = x => {
    val value = if(x.isEmpty) 0 else x.toInt
    if (value < 0 || value > 3) throw new PropertyException(s"illegal value for boolean property `$value`")
    value match {
      case 0 => Empty
      case 1 => Positive
      case 2 => Negative
      case 3 => Tau
    }
  }
}
