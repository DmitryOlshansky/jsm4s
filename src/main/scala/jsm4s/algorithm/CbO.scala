package jsm4s.algorithm

import jsm4s.ds.FcaSet
import jsm4s.property.Properties

import scala.collection.{Seq, mutable}

abstract class CbO(rows: Seq[FcaSet], props: Seq[Properties],
                   attrs: Int, minSupport: Int,
                   stats: StatsCollector, sink: Sink)
  extends Algorithm(rows, props, attrs, minSupport, stats, sink) {

  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    output(A, B)
    var j = y
    while (j < attributes) {
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        stats.onClosure()
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.until(j) == D.until(j)) method(C, D, j + 1)
          else stats.onCanonicalTestFailure()
        }
      }
      j += 1
    }
  }

  override def perform = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    method(A, B, 0)
  }
}

trait GenericBCbO extends QueueAlgorithm {

  var recDepth = 0

  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    output(A, B)
    val q = mutable.Queue[(FcaSet, FcaSet, Int)]()
    for (j <- y until attributes) {
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.until(j) == D.until(j)) q.enqueue((C, D, j + 1))
          else stats.onCanonicalTestFailure()
        }
      }
    }
    recDepth += 1
    while (!q.isEmpty) processQueue(q.dequeue())
    recDepth -= 1
  }

  override def perform = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    method(A, B, 0)
  }
}