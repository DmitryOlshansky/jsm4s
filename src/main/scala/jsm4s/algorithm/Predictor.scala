package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import jsm4s.algorithm.Strategies.MergeStrategy
import jsm4s.ds.FcaSet
import jsm4s.property.Properties

class Predictor(val hypotheses: Seq[Hypothesis], val attrs: Int, val mergeStrategy: MergeStrategy)
  extends LazyLogging {

  private val ladder = Ladder(hypotheses, attrs)

  def apply(example: FcaSet): Properties = {
    val hyps = ladder.search(example)
    val matching = hyps.filter {
      h => h.intent.subsetOf(example, attrs)
    }
    mergeStrategy(matching.map(_.props))
  }
}