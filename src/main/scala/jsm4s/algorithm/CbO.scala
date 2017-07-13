package jsm4s.algorithm

import java.util.concurrent.{Executors, TimeUnit}

import jsm4s.ds.FcaSet

import scala.collection.{Seq, mutable}

abstract class CbO(rows: Seq[FcaSet], attrs: Int, minSupport: Int, properties: Int)
  extends Algorithm(rows, attrs, minSupport, properties) {

  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    if (!output(A, B)) return
    var j = y
    while (j < attributes) {
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        onClosure()
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.until(j) == D.until(j)) method(C, D, j + 1)
          else onCanonicalTestFailure()
        }
      }
      j += 1
    }
  }

  override def perform = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    method(A, B, 0)
    flush()
  }
}

trait GenericBCbO extends GenericAlgorithm {

  var recDepth = 0

  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    if (!output(A, B)) return
    val q = mutable.Queue[(FcaSet, FcaSet, Int)]()
    for (j <- y until attributes) {
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.until(j) == D.until(j)) q.enqueue((C, D, j + 1))
          else onCanonicalTestFailure()
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