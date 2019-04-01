package jsm4s.algorithm

import jsm4s.property.{BinaryProperty, NullProperty, Properties, Property}

import scala.collection.mutable

object Strategies {

  type MergeStrategy = Seq[Property] => Property

  def votingMajority(seq: Seq[Property]): Property = {
    if (seq.isEmpty) NullProperty
    else seq.head match {
      case _: BinaryProperty  =>
        var votes = 0
        for(i <- seq.indices) {
          if (seq(i).asInstanceOf[BinaryProperty].positive) votes += 1
          else votes -= 1
        }
        if (votes > 0) BinaryProperty.Positive
        else if(votes == 0) BinaryProperty.Tau
        else BinaryProperty.Negative
    case head: Properties => // generic properties code
      val len = head.size
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
