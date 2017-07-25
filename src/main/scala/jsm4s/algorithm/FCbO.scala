package jsm4s.algorithm

import java.util.concurrent.{ForkJoinPool, TimeUnit}

import jsm4s.ds.FcaSet
import jsm4s.property.Properties

import scala.collection.Seq

abstract class GenericFCbO(
                            rows: Seq[FcaSet], props: Seq[Properties],
                            attributes: Int, minSupport: Int,
                            stats: StatsCollector, sink: Sink
                          )
  extends Algorithm(rows, props, attributes, minSupport, stats, sink) with QueueAlgorithm {

  var recDepth = 0

  def method(A: FcaSet, B: FcaSet, y: Int, errors: Array[FcaSet], N: Int): Unit = {
    val q = Array.ofDim[(FcaSet, FcaSet, Int, Array[FcaSet], Int)](attributes - y)
    var top = 0
    output(A, B)
    val M = N + attributes
    var j = y
    while (j < attributes) {
      errors(M + j) = errors(N + j)
      if (!B.contains(j)) {
        if (errors(N + j).subsetOf(B, j)) {
          val ret = closeConcept(A, j)
          stats.onClosure()
          if (ret._1) {
            val C = ret._2
            val D = ret._3
            if (B.until(j) == D.until(j)) {
              q(top) = ((C, D, j + 1, errors, M))
              top += 1
            }
            else {
              errors(M + j) = D
              stats.onCanonicalTestFailure()
            }
          }
        }
      }
      j += 1
    }
    recDepth += 1
    var k = 0
    while (k < top) {
      processQueue(q(k))
      k += 1
    }
    recDepth -= 1
  }

  override def perform = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    val implied = Array.ofDim[FcaSet]((attributes + 1) * attributes)
    for (i <- 0 until ((attributes + 1) * attributes)) implied(i) = emptyIntent
    method(A, B, 0, implied, 0)
  }
}

abstract class FCbO(rows: Seq[FcaSet], props:Seq[Properties],
                    attrs: Int, minSupport: Int,
                    stats: StatsCollector, sink: Sink
                   )
  extends GenericFCbO(rows, props, attrs, minSupport, stats, sink) {
  def processQueue(value: AnyRef): Unit = {
    val x = value.asInstanceOf[(FcaSet, FcaSet, Int, Array[FcaSet], Int)]
    method(x._1, x._2, x._3, x._4, x._5)
  }
}

abstract class PFCbO(rows: Seq[FcaSet], props: Seq[Properties],
                     attrs: Int, minSupport: Int,
                     stats: StatsCollector, sink: Sink)
  extends GenericFCbO(rows, props, attrs, minSupport, stats, sink)  {

  private val pool = ForkJoinPool.commonPool

  override def processQueue(value: AnyRef) = {
    val tup = value.asInstanceOf[(FcaSet,FcaSet,Int,Array[FcaSet], Int)]
    pool.submit(new Runnable {
      override def run(): Unit = method(tup._1, tup._2, tup._3, tup._4, tup._5)
    })
  }

  override def perform = {
    super.perform()
    pool.awaitQuiescence(1000, TimeUnit.DAYS)
  }
}