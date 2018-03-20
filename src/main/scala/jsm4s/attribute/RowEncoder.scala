package jsm4s.attribute

class RowEncoder(encoders: Seq[Encoder]) {
  def apply(row: Seq[Any]) = {
    row.zip(encoders).flatMap {
      case (value, enc) => enc(value)
    }
  }

  val size = encoders.foldLeft(0)(_ + _.size)
}
