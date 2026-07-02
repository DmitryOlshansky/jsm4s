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
                for (h <- predictor.matching(p._1)) {
                    if(h.props != p._2) {
                        cost += 1
                    }
                }
            }
        }
        cost
    }

    def badHypotheses(testHypotheses: Seq[Hypothesis]): Seq[Hypothesis] = {
        val predictor = new Predictor(hypotheses, attrs, factory, mergeStrategy)
        val predictions = train.par.map { e => (e._1, e._2, predictor(e._1)) }.seq
        val bads = mutable.Buffer[Hypothesis]()
        for (p <- predictions) {
            if (p._2 != p._3) {
                for (h <- predictor.matching(p._1)) {
                    if(h.props != p._2) {
                        bads.append(h)
                    }
                }
            }
        }
        bads
    }

    def tune(): Seq[Hypothesis] = {
        var curatedHypotheses = hypotheses.filter { _.intent.count(_ >= 0) > 5 }
        var process = true
        while (process) { 
            var bestCost = computeCost(curatedHypotheses)
            val startingCost = bestCost
            var bestSize = Integer.MAX_VALUE
            val bads = badHypotheses(curatedHypotheses).sortBy { _.intent.count(_ >= 0) }
            var j = -1
            for (i <- bads.indices) {
                val bad = bads(i)
                val size = bad.intent.count(_ >= 0)
                val testHypotheses = curatedHypotheses.filter(x => !(x.intent == bad.intent)).toSeq
                if (testHypotheses.size < curatedHypotheses.size) {
                    val cost = computeCost(testHypotheses)
                    if (cost < bestCost || (cost == bestCost && size < bestSize)) {
                        bestCost = cost
                        bestSize = size
                        j = i
                    }
                }
            }
            if (j == -1 || startingCost < 2500) process = false;
            else {
                logger.debug("Trimmed hypothesis cost before = {} cost after = {}", startingCost, bestCost)
                curatedHypotheses = curatedHypotheses.filter(x => !(x.intent == bads(j).intent)).toSeq
            }
        }
        curatedHypotheses
    }
}
