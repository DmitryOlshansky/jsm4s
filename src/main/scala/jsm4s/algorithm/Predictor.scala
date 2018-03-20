package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.FcaSet
import jsm4s.property.Properties
import jsm4s.Utils._
import jsm4s.algorithm.Strategies.MergeStrategy

import scala.collection.mutable

case class Tree(splits: Seq[(Int, Seq[Hypothesis])], rem: Seq[Hypothesis])

class Predictor(val hypotheses: Seq[Hypothesis], val attrs: Int, val mergeStrategy: MergeStrategy)
extends LazyLogging {

  private def buildTree: Tree = {
    var remaining = hypotheses
    val weight = Array.ofDim[Int](attrs)
    timeIt("Weight calculation") {
      var i = 0
      while(i < hypotheses.size) {
        val h = hypotheses(i)
        for (i <- h.intent) {
          weight(i) += 1
        }
        i += 7
      }
    }
    val sorted = timeIt("Hypotheses sorting")(weight.zipWithIndex.filter(_._1 > 20).sortWith((a, b) => a._1 < b._1))
    val topAttrs = sorted.map(_._2)
    timeIt("Tree building") {
      val buf = mutable.Buffer[(Int, Seq[Hypothesis])]()
      for (j <- topAttrs) {
        val parts = remaining.partition { x => x.intent.contains(j) }
        buf += j -> parts._1
        remaining = parts._2
      }
      logger.info("Reminder size is {}", remaining.size)
      Tree(buf, remaining)
    }
  }

  private val tree = buildTree

  def search(example: FcaSet): Seq[Hypothesis] = tree.splits.flatMap(p => if (example.contains(p._1)) p._2 else Seq()) ++ tree.rem

  def apply(example: FcaSet):Properties = {
    val hyps = search(example)
    val matching = hyps.filter{
      h => h.intent.subsetOf(example, attrs)
    }
    println(s"Example: $example")
    for (h <- matching) {
      println(s"\t${h.intent} ${h.props}")
    }
    mergeStrategy(matching.map(_.props))
  }
}
