package jsm4s.property

import scala.collection.SortedSet

trait PropertyEncoder {
  def apply(value: Any): Property
}

class BinaryPropertyEncoder(values: SortedSet[String]) extends PropertyEncoder {
  if (values.size != 2) throw PropertyException(s"Illegal number of values for binary property `${values.size}")
  val first = values.head
  val second = values.last

  def apply(x: Any) = {
    if (x  == first) new BinaryProperty(1)
    else if(x == second) new BinaryProperty(2)
    else throw PropertyException(s"Not supported type ${x.getClass} for binary property")
  }
}

class PropertiesEncoder(encoders: Seq[PropertyEncoder]) {
  def apply(values: Seq[Any]): Properties = new Properties(
    values.zip(encoders).map {
      case (value, enc) => enc(value)
    }
  )
}