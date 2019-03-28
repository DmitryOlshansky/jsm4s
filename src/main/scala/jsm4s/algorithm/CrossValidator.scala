

package jsm4s.algorithm

import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.FcaSet
import jsm4s.property.Properties
import jsm4s.algorithm.Strategies.MergeStrategy

class CrossValidator(val hypotheses: Seq[Hypothesis], val attrs: Int, val mergeStrategy: MergeStrategy)
  extends LazyLogging {

  private val ladder = Ladder(hypotheses, attrs)

  private val counters = Array.fill(hypotheses.size)(new AtomicInteger())

  def apply(example: FcaSet, expected: Properties): Boolean = {
    val hyps = ladder.search(example)
    val matching = hyps.filter {
      h => h.intent.subsetOf(example, attrs)
    }
    val result = mergeStrategy(matching.map(_.props))
    if ((result & expected).empty) false
    else true
  }

}
