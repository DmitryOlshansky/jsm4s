package jsm4s.algorithm

import jsm4s.ds.FcaSet
import jsm4s.property.Properties

case class Hypothesis(intent: FcaSet, props: Properties)

trait HasIntent[T] {
  def intent(value: T): FcaSet
}

object Hypothesis {
  implicit val hasIntent = new HasIntent[Hypothesis] {
    override def intent(value: Hypothesis): FcaSet = value.intent
  }
}

case class IndexedHypothesis(intent: FcaSet, props: Properties, index: Int)

object IndexedHypothesis {
  implicit val hasIntent = new HasIntent[IndexedHypothesis] {
    override def intent(value: IndexedHypothesis): FcaSet = value.intent
  }
}