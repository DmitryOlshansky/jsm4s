package jsm4s.algorithm

import jsm4s.property.{BinaryProperty, Properties, Property}

import scala.collection.mutable

object Strategies {

  type MergeStrategy = Seq[Properties] => Properties

  def votingMajority(seq: Seq[Properties]): Properties = {
    if (seq.isEmpty) Properties(Seq.empty)
    else if (seq.head.value.head.isInstanceOf[BinaryProperty]) {
      val len = seq.head.size
      val votes = Array.fill(len)(0)
      seq.foreach {
        case Properties(props) =>
          for (i <- props.indices) {
            if (props(i).asInstanceOf[BinaryProperty].positive) votes(i) += 1
            else votes(i) -= 1
          }
      }
      Properties(votes.map { x =>
        if(x > 0) new BinaryProperty(1)
        else if (x < 0) new BinaryProperty(2)
        else new BinaryProperty(3)
      })
    }
    else { // generic property code
      val len = seq.head.size
      val votes = Array.fill(len)(mutable.Map[Property, Int]())
      seq.foreach {
        case Properties(props) =>
          for (i <- 0 until len) {
            votes(i).put(props(i), 1 + votes(i).getOrElse(props(i), 0))
          }
      }
      Properties(votes.map { x => x.maxBy(pair => pair._2)._1 })
    }
  }

}
