package jsm4s.algorithm

import jsm4s.JsmException
import jsm4s.property.{BinaryProperty, OrdinalProperty, Composite, Property}
import jsm4s.Utils.ensure

import scala.collection.mutable

object Strategies {

  type MergeStrategy = Seq[Property] => Property

  def noCounterExamples(seq: Seq[Property]): Property = {
    seq.reduceLeft(_ & _)
  }

  def boundedVotingMajority(bound: Int)(seq: Seq[Property]): Property = {
    ensure (seq.nonEmpty, new JsmException("merge strategies do not accept empty list"))
    seq.head match {
      case _: BinaryProperty  =>
        var votes = 0
        for(i <- seq.indices) {
          if (seq(i).asInstanceOf[BinaryProperty].positive) votes += 1
          else votes -= 1
        }
        if (votes > seq.length - bound) BinaryProperty.Positive
        else if (votes < -seq.length + bound) BinaryProperty.Negative
        else BinaryProperty.Empty
      case _: OrdinalProperty => 
        if (seq.length > 1) {
          var h = mutable.Map[Int, Int]()
          for (p <- seq) {
            val ord = p.asInstanceOf[OrdinalProperty].value
            val x = h.get(ord)
            x match {
              case None => h.put(ord, 1)
              case Some(v) => h.put(ord, v+1)
            }
          }
          val votes = h.seq
          val max = votes.maxBy(_._2)
          if (max._2 > seq.length - bound)
            new OrdinalProperty(max._1)
          else
            OrdinalProperty.Empty
        }
        else seq.head
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

  def noop(seq: Seq[Property]): Property = seq.head

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
      case _: OrdinalProperty => 
        if (seq.length > 1) {
          var h = mutable.Map[Int, Int]()
          for (p <- seq) {
            val ord = p.asInstanceOf[OrdinalProperty].value
            val x = h.get(ord)
            x match {
              case None => h.put(ord, 1)
              case Some(v) => h.put(ord, v+1)
            }
          }
          new OrdinalProperty(h.seq.maxBy(_._2)._1)
        }
        else seq.head
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
