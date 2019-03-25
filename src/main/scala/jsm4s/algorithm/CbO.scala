package jsm4s.algorithm

import java.util.concurrent.{ForkJoinPool, TimeUnit}

import jsm4s.ds._

import scala.collection.mutable

class CbO(context: Context) extends Algorithm(context) {

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
    val A = ext.full
    val B = rows.fold(int.full)((a, b) => a & b) // full intersection
    method(A, B, 0)
  }
}

abstract class GenericBCbO(context: Context)
              extends Algorithm(context) with QueueAlgorithm[(FcaSet,FcaSet,Int)] {

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
    val A = ext.full
    val B = rows.fold(int.full)((a, b) => a & b) // full intersection
    method(A, B, 0)
  }
}

class PCbO(context: Context, threads: Int) extends GenericBCbO(context) {

  private val pool = if (threads == 0) ForkJoinPool.commonPool else new ForkJoinPool(threads)

  override def processQueue(tup: (FcaSet,FcaSet,Int)) = {
    pool.submit(new Runnable {
      override def run(): Unit = method(tup._1, tup._2, tup._3)
    })
  }

  override def perform = {
    super.perform()
    pool.awaitQuiescence(1000, TimeUnit.DAYS)
  }
}