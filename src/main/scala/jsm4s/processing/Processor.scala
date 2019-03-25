package jsm4s.processing

import jsm4s.algorithm.{Context, Hypothesis, Sink}
import jsm4s.ds.{FcaSet, IntentFactory}

abstract class Processor(sink: Sink) extends Sink {
  def preProcess(intent: FcaSet): FcaSet

  def apply(hypothesis: Hypothesis):Unit = sink(hypothesis)

  def close():Unit = sink.close()
}

class IdentityProcessor(sink:Sink) extends Processor(sink) {
  override def preProcess(intent: FcaSet): FcaSet = intent
}

class SortingProcessor(rows: Seq[FcaSet], attributes: Int, sink: Sink, int: IntentFactory) extends Processor(sink) {
  val objects = rows.size
  val weights = Array.ofDim[Int](attributes)
  for (r <- rows; j <- r) {
    weights(j) += 1
  }
  val order = weights.zipWithIndex.sortBy(_._1).map(_._2)
  val revMap = Array.ofDim[Int](attributes)
  for (a <- 0 until attributes) revMap(order(a)) = a

  def preProcess(intent: FcaSet): FcaSet = {
    int.values(intent.map(revMap))
  }

  def postProcess(intent: FcaSet) = int.values(intent.map(order))

  override def apply(hyp: Hypothesis): Unit = {
    sink.apply(hyp.copy(intent = int.values(hyp.intent.map(order))))
  }
}
