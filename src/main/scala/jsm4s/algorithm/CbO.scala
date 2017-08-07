package jsm4s.algorithm

import java.util.concurrent.{ForkJoinPool, TimeUnit}

import jsm4s.ds.{BitSet, FcaSet, SparseBitSet}
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
          if (B.equalUpTo(D, j)) method(C, D, j + 1)
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

abstract class GenericBCbO(
                        rows: Seq[FcaSet], props: Seq[Properties],
                        attributes: Int, minSupport: Int,
                        stats: StatsCollector, sink: Sink)
  extends Algorithm(rows, props, attributes, minSupport, stats, sink) with QueueAlgorithm {

  var recDepth = 0

  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    output(A, B)
    val q = mutable.Queue[(FcaSet, FcaSet, Int)]()
    var j = y
    while(j < attributes){
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.equalUpTo(D, j)) q.enqueue((C, D, j + 1))
          else stats.onCanonicalTestFailure()
        }
      }
      j += 1
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

abstract class PCbO(rows: Seq[FcaSet], props: Seq[Properties],
                    attrs: Int, minSupport: Int,
                    stats: StatsCollector, sink: Sink)
  extends GenericBCbO(rows, props, attrs, minSupport, stats, sink) {

  private val pool = ForkJoinPool.commonPool

  override def processQueue(value: AnyRef) = {
    val tup = value.asInstanceOf[(FcaSet,FcaSet,Int)]
    pool.submit(new Runnable {
      override def run(): Unit = method(tup._1, tup._2, tup._3)
    })
  }

  override def perform = {
    super.perform()
    pool.awaitQuiescence(1000, TimeUnit.DAYS)
  }
}