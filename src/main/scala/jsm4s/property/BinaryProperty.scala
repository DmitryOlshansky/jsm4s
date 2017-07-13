package jsm4s.property

class BinaryProperty(val value: Int) extends Property {

  def positive = (value & 1) != 0

  def negative = (value & 2) != 0

  def &(prop: BinaryProperty) = new BinaryProperty(value & prop.value)

  override def tau: Boolean = value == 3

  override def empty: Boolean = value == 0

  override def mkString: String = value.toString

  override def equals(o: scala.Any): Boolean = value == o.asInstanceOf[BinaryProperty].value
}

object BinaryProperty{
  def intersection(props: BinaryProperty*): BinaryProperty =
    new BinaryProperty(props.foldLeft(3)((a, x) => a & x.value))

  def factory:Property.Factory = (x) => {
    val value = x.toInt
    if (value < 0 || value > 3) throw new PropertyException(s"illegal value for boolean property `$value`")
    new BinaryProperty(value)
  }
}
