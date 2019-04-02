package jsm4s.algorithm

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.FcaSet
import jsm4s.property.{Property, PropertyFactory}
import jsm4s.Utils._
import jsm4s.algorithm.Strategies.MergeStrategy

import scala.collection.mutable


class Predictor(val hypotheses: Seq[Hypothesis], val attrs: Int, factory: PropertyFactory, val mergeStrategy: MergeStrategy)
  extends (FcaSet => Property) with LazyLogging {

  private val search = Ladder(hypotheses, attrs)

  def apply(example: FcaSet): Property = {
    val hyps = search(example)
    val matching = hyps.filter {
      h => h.intent.subsetOf(example, attrs)
    }
    if (matching.isEmpty) factory.empty
    else mergeStrategy(matching.map(_.props))
  }
}
