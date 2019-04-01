package jsm4s.property

import jsm4s.Utils.ensure
import scala.collection.mutable

case class Composite(value: Seq[Property]) extends Property {

  override def &(props: Property): Composite = {
    val rhs = props.asInstanceOf[Composite].value
    val result = Array.ofDim[Property](rhs.size)
    for (i <- result.indices) {
      result(i) = value(i) & rhs(i)
    }
    Composite(result)
  }

  override def empty = !value.exists(p => !p.empty)

  def size = value.size

  override def tau: Boolean = value.nonEmpty && !value.exists(p => !p.tau)

  override def toString = value.mkString(" ")

}

object Composite {

  private def parse(onBinary: Seq[String] => Unit)(description: String) = {
    var i = 0

    while (i < description.length) {
      val head = description.charAt(i)
      head match {
        case 'B' =>
          i += 1
          ensure(description.charAt(i) == '(', new PropertyException("Expected '(' in binary property marker"))
          i += 1
          val j = i
          while(i < description.length && description.charAt(i) != ')') i += 1
          onBinary(description.slice(j, i).split(",").map(_.trim))
        case _ => throw new PropertyException(s"Unsupported property type marker `$head`")
      }
      i += 1
    }
  }

  class Factory(description: String) extends PropertyFactory {
    val (factories, tau, empty)  = {
      val buf = mutable.Buffer[PropertyFactory]()
      parse(
        keys => buf += new BinaryProperty.Factory(keys)
      )(description)
      (buf.toSeq, new Composite(buf.map(_.tau)), new Composite(buf.map(_.empty)))
    }

    def encode(x: String) = {
      val items = x.split(" ")
      val props = items.zipWithIndex.map { pair =>
        factories(pair._2).encode(pair._1)
      }
      new Composite(props)
    }

    def decode(p: Property): String = {
      val composite = p.asInstanceOf[Composite]
      factories.zip(composite.value).map { case (f, prop) =>
        f.decode(prop)
      }.mkString(" ")
    }
  }
}