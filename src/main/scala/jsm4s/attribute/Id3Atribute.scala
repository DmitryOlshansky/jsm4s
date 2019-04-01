package jsm4s.attribute

class Id3Atribute(values: Map[String, Map[String, Int]], offset: Int)
extends Attribute {

  val splitter = new Id3Partitioner(
    values.toArray.map {
      case (value, counts) =>
        Id3Point(value.toDouble, counts)
    }.sortBy(_.value)
  )

  override def apply(value: String): Seq[Int] = {
    val idx = splitter.findPartition(value.toDouble)
    val cover = if (idx == 0) Seq(idx, idx+1)
    else if(idx == splitter.numPartitions-1) Seq(idx-1, idx)
    else Seq(idx -1, idx, idx + 1)
    cover.map(_ + offset)
  }

  override def size: Int = splitter.numPartitions
}
