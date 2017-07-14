package jsm4s.property

import scala.collection.mutable

case class Properties(val value: Seq[Property]){

  def &(props: Properties): Properties = Properties(value.zip(props.value).map(p => p._1 & p._2))

  def empty = !value.exists(p => !p.empty)

  def size = value.size

  def tau: Boolean = !value.exists(p => !p.tau)

  override def toString = value.mkString(" ")
}

object Properties {
  type TauFactory = () => Properties
  type Factory = String => Properties

  private def parse(onBinary:()=>Unit, onLevel:(Int)=>Unit)(description: String) = {
    var i = 0

    while (i < description.length) {
      val head = description.charAt(i)
      val factory = head match {
        case 'B' => onBinary()
        case 'L' =>
          val j = i + 1
          while(i < description.length && Character.isDigit(description.charAt(i))) {
            i += 1
          }
          val levels = description.slice(j, i).toInt
          onLevel(levels)
        case _ => throw new PropertyException(s"Unsupported property type marker `$head`")
      }
      i += 1
    }

  }

  def loader(description: String): Factory = {
    val factories = mutable.Buffer[Property.Factory]()
    parse(
      () => factories += BinaryProperty.loader,
      (levels) => factories += LevelProperty.loader(levels)
    )(description)
    (x) => {
      val props = x.split(" ").zipWithIndex.map { pair =>
        factories(pair._2)(pair._1)
      }
      new Properties(props)
    }
  }

  def tau(description: String):Properties = {
    val properties = mutable.Buffer[Property]()
    parse(
      () => properties += BinaryProperty.tau,
      (levels) => properties += LevelProperty.tau(levels)
    )(description)
    new Properties(properties)
  }
}