package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.FcaSet
import jsm4s.property.{Property, PropertyFactory}
import jsm4s.Utils._
import jsm4s.algorithm.Strategies.MergeStrategy

import scala.collection.mutable


class CrossValidator(val hypotheses: Seq[Hypothesis], val attrs: Int, factory: PropertyFactory, val mergeStrategy: MergeStrategy)
  extends ((FcaSet,Property) => Boolean) with LazyLogging {

  private val search = Ladder(hypotheses, attrs)

  def apply(example: FcaSet, real: Property): Boolean = {
    val hyps = search(example)
    val matching = hyps.filter {
      h => h.intent.subsetOf(example, attrs)
    }
    val predicted = if (matching.isEmpty) factory.empty else mergeStrategy(matching.map(_.props))
    if ((predicted & real).empty) {
      matching.foreach { hyp =>
        if ((hyp.props & real).empty) {
          //logger.info(s"Wrong $hyp")
          hyp.wrong.incrementAndGet()
        }
        else if ((hyp.props & predicted).empty) {
          hyp.correct.incrementAndGet()
          //logger.info(s"Correct $hyp")
        }
      }
    }
    else {
      matching.foreach { hyp =>
        if ((hyp.props & real).empty) {
          //logger.info(s"Wrong $hyp")
          hyp.wrong.incrementAndGet()
        }
        else if ((hyp.props & predicted).empty) {
          hyp.correct.incrementAndGet()
          //logger.info(s"Correct $hyp")
        }
      }
    }
    !(predicted & real).empty
  }
}
