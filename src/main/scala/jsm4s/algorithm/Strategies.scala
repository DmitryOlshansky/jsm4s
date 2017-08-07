package jsm4s.algorithm

import jsm4s.property.{Properties, Property}

import scala.collection.mutable

object Strategies {

  type MergeStrategy = Seq[Properties] => Properties

  def votingMajority(seq: Seq[Properties]): Properties = {
    if (seq.isEmpty) Properties(Seq())
    else {
      val len = seq.head.size
      val votes = Array.fill(len)(mutable.Map[Property, Int]())
      seq.foreach {
        case Properties(props) =>
          for (i <- 0 until len) {
            votes(i).put(props(i), 1 + votes(i).getOrElse(props(i), 0))
          }
      }
      new Properties(votes.map { x => x.maxBy(pair => pair._2)._1 })
    }
  }

}
