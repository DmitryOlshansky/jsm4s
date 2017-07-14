package jsm4s.property

import scala.collection.SortedSet

class BinaryProperty(val value: Int) extends Property {

  def positive = (value & 1) != 0

  def negative = (value & 2) != 0

  override def &(prop: Property) = new BinaryProperty(value & prop.asInstanceOf[BinaryProperty].value)

  override def tau: Boolean = value == 3

  override def empty: Boolean = value == 0

  override def toString: String = value.toString

  override def equals(o: scala.Any): Boolean = value == o.asInstanceOf[BinaryProperty].value
}

object BinaryProperty{
  val tau = new BinaryProperty(3)

  def intersection(props: BinaryProperty*): BinaryProperty =
    new BinaryProperty(props.foldLeft(3)((a, x) => a & x.value))

  def factory(values: SortedSet[String]):Property.Factory = {
    if (values.size != 2) throw  new PropertyException(s"Illegal number of values for bianry property `${values.size}")
    val first = values.head
    (x) => {
      if (x == first) new BinaryProperty(1)
      else new BinaryProperty(2)
    }
  }

  def loader:Property.Factory = (x) => {
    val value = x.toInt
    if (value < 0 || value > 3) throw new PropertyException(s"illegal value for boolean property `$value`")
    new BinaryProperty(value)
  }
}
