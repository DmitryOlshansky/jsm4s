package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import collection.mutable

import jsm4s.ds.FcaSet
import jsm4s.property.Property
import jsm4s.algorithm.Strategies.MergeStrategy
import jsm4s.property.PropertyFactory

class Tuner(
    val hypotheses: Seq[Hypothesis], 
    val attrs: Int,
    val factory: PropertyFactory,
    val mergeStrategy: MergeStrategy,
    val train: Seq[(FcaSet, Property)]
) extends LazyLogging  {

    private def computeCost(testHypotheses: Seq[Hypothesis]): Int = {
        val predictor = new Predictor(testHypotheses, attrs, factory, mergeStrategy)
        val predictions = train.par.map { e => (e._1, e._2, predictor(e._1)) }.seq
        var cost = 0
        for (p <- predictions) {
            if (p._2 != p._3) {
                cost += 1000
                val counts = mutable.HashMap[Property, Int]()
                for (h <- predictor.matching(p._1)) {
                    if(h.props != p._2) {
                        cost += 1
                    }
                }
            }
        }
        cost
    }

    def tune(): Seq[Hypothesis] = {
        val predictor = new Predictor(hypotheses, attrs, factory, mergeStrategy)
        val predictions = train.par.map { e => (e._1, e._2, predictor(e._1)) }.seq
        val badHypotheses = mutable.Buffer[Hypothesis]()
        for (p <- predictions) {
            if (p._2 != p._3) {
                val counts = mutable.HashMap[Property, Int]()
                for (h <- predictor.matching(p._1)) {
                    if(h.props != p._2) {
                        badHypotheses.append(h)
                    }
                }
            }
        }
        var curatedHypotheses = hypotheses
        var bestCost = computeCost(curatedHypotheses)
        for (i <- badHypotheses.indices) {
            val bad = badHypotheses(i)
            val testHypotheses = curatedHypotheses.filter(_ != bad).toSeq
            val cost = computeCost(testHypotheses)
            logger.debug("Trying to trim hypothesis {}/{} cost before = {} cost after = {}", i, badHypotheses.size, bestCost, cost)
            if (cost < bestCost) {
                bestCost = cost
                curatedHypotheses = testHypotheses
            }
        }
        curatedHypotheses
    }
}
