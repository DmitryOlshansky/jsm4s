package jsm4s.property

class LevelProperty(val levels: Int, val positive:Int, val negative:Int) extends Property {

  def &(prop: LevelProperty):LevelProperty = {
    // ++++++00 1st
    // 0000---- 2nd
    // ++++00-- result
    val pos1 = Math.min(positive, levels - Math.max(negative, prop.negative))
    val pos2 = Math.min(prop.positive, levels - Math.max(negative, prop.negative))
    // Ignore zero levels
    val pos = if (pos1 == 0) pos2 else if (pos2 == 0) pos1 else Math.min(pos1, pos2)

    val neg1 = Math.min(negative, levels - Math.max(positive, prop.positive))
    val neg2 = Math.min(prop.negative, levels - Math.max(positive, prop.positive))
    // Ignore zero levels
    val neg = if (neg1 == 0) neg2 else if (neg2 == 0) neg1 else Math.min(neg1, neg2)

    new LevelProperty(levels, pos, neg)
  }

  override def tau: Boolean = positive == levels && negative == levels

  override def empty: Boolean = positive == 0 && negative == 0

  override def mkString: String = s"$positive:$negative"

  override def equals(o: scala.Any): Boolean = {
    val that = o.asInstanceOf[LevelProperty]
    that.levels == levels && that.positive == positive && that.negative == negative
  }
}

object LevelProperty{
  def intersection(props: Iterable[LevelProperty]): LevelProperty = props.reduceLeft((a, b) => a & b)

  def factory(levels: Int):Property.Factory = (x) => {
    val parts = x.split(":")
    new LevelProperty(levels, parts(0).toInt, parts(1).toInt)
  }
}
