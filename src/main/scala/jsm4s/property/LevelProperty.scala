package jsm4s.property

class LevelProperty(val levels: Int, val positive:Int, val negative:Int) extends Property {

  def &(property: Property):LevelProperty = {
    val prop = property.asInstanceOf[LevelProperty]
    // ++++++00 1st
    // 0000---- 2nd
    // ++++00-- result
    val pos1 = Math.min(positive, levels - Math.max(negative, prop.negative))
    val pos2 = Math.min(prop.positive, levels - Math.max(negative, prop.negative))
    val pos = Math.max(pos1, pos2)

    val neg1 = Math.min(negative, levels - Math.max(positive, prop.positive))
    val neg2 = Math.min(prop.negative, levels - Math.max(positive, prop.positive))
    val neg = Math.max(neg1, neg2)

    new LevelProperty(levels, pos, neg)
  }

  override def tau: Boolean = positive == levels && negative == levels

  override def empty: Boolean = positive == 0 && negative == 0

  override def toString: String = s"$positive:$negative"

  override def equals(o: scala.Any): Boolean = {
    val that = o.asInstanceOf[LevelProperty]
    that.levels == levels && that.positive == positive && that.negative == negative
  }
}

object LevelProperty{

  def tau(levels: Int) = new LevelProperty(levels, levels, levels)

  def loader(levels: Int):Property.Factory = (x) => {
    if(x.isEmpty) new LevelProperty(levels, 0, 0)
    else {
      val parts = x.split(":")
      new LevelProperty(levels, parts(0).toInt, parts(1).toInt)
    }
  }
}
