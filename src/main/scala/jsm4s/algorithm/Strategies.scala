package jsm4s.algorithm

import jsm4s.JsmException
import jsm4s.property.{BinaryProperty, Composite, Property}
import jsm4s.Utils.ensure

import scala.collection.mutable

object Strategies {

  type MergeStrategy = Seq[Property] => Property

  def noCounterExamples(seq: Seq[Property]): Property = {
    seq.reduceLeft(_ & _)
  }

  def votingMajority(seq: Seq[Property]): Property = {
    ensure (seq.nonEmpty, new JsmException("merge strategies do not accept empty list"))
    seq.head match {
      case _: BinaryProperty  =>
        var votes = 0
        for(i <- seq.indices) {
          if (seq(i).asInstanceOf[BinaryProperty].positive) votes += 1
          else votes -= 1
        }
        if (votes > seq.length/2) BinaryProperty.Positive
        else if (votes < -seq.length/2) BinaryProperty.Negative
        else BinaryProperty.Empty
      case head: Composite => // generic properties code
        val len = head.size
        val votes = Array.fill(len)(mutable.Map[Property, Int]())
        seq.foreach {
          case Composite(props) =>
            for (i <- 0 until len) {
              votes(i).put(props(i), 1 + votes(i).getOrElse(props(i), 0))
            }
        }
        Composite(votes.map { x => x.maxBy(pair => pair._2)._1 })
    }
  }

}
