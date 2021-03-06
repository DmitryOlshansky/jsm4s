package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.FcaSet
import jsm4s.property.{Property, PropertyFactory}
import jsm4s.Utils._
import jsm4s.algorithm.Strategies.MergeStrategy

import scala.collection.mutable

// Ladder overview:
// first element - hypotheses that contain given attribute
// second - that contain given attribute but not the one from first split
// third - doesn't contain 1 & 2 but contains yet another attribute
// ...
// ladder is constructed by going this way from less popular to more popular attributes
//
case class Ladder(steps: Seq[(Int, Seq[Hypothesis])], rem: Seq[Hypothesis])

class Predictor(val hypotheses: Seq[Hypothesis], val attrs: Int, factory: PropertyFactory, val mergeStrategy: MergeStrategy)
  extends (FcaSet => Property) with LazyLogging {


  private def buildLadder: Ladder = {
    val portion = 0.005 // exclude all attributes that sum up to less then `portion` of total
    val threshold = 20 // minimum weight to consider, trims down on fruitless computation
    var remaining = hypotheses
    val weight = Array.ofDim[Int](attrs)
    var total = 0.0
    timeIt("Weights calculation") {
      var i = 0
      while(i < hypotheses.size) {
        val h = hypotheses(i)
        for (i <- h.intent) {
          weight(i) += 1
          total += 1
        }
        i += 7
      }
    }
    val sorted = timeIt("Weights sorting")(weight.zipWithIndex.filter(_._1 > threshold).sortWith((a, b) => a._1 < b._1))
    val cumulative = sorted.map(_._1 / total).scanLeft(0.0)((acc, x) => acc + x)
    val significant = cumulative.indexWhere(_ > portion)
    val topAttrs = sorted.slice(significant, sorted.length).map(_._2)
    timeIt("Ladder building") {
      val buf = mutable.Buffer[(Int, Seq[Hypothesis])]()
      for (j <- topAttrs) {
        if (remaining.nonEmpty) {
          val parts = remaining.partition { x => x.intent.contains(j) }
          buf += j -> parts._1
          remaining = parts._2
        }
      }
      logger.info("Reminder size is {}", remaining.size)
      Ladder(buf, remaining)
    }
  }

  private val ladder = buildLadder

  def search(example: FcaSet): Seq[Hypothesis] = {
    val collected = ladder.steps.flatMap(p => if (example.contains(p._1)) p._2 else Seq.empty)
    if (ladder.rem.nonEmpty) collected ++ ladder.rem
    else collected
  }

  def apply(example: FcaSet): Property = {
    val hyps = search(example)
    val matching = hyps.filter {
      h => h.intent.subsetOf(example, attrs)
    }
    if (matching.isEmpty) factory.tau
    else mergeStrategy(matching.map(_.props))
  }
}
