package jsm4s.algorithm

import java.util.concurrent.{ForkJoinPool, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
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

class PCbO(context: Context, threads: Int) extends Algorithm(context) {

  private val pool = if (threads == 0) ForkJoinPool.commonPool else new ForkJoinPool(threads)
  private val cores = pool.getParallelism()
  private val submitted = new AtomicInteger(0)
  
  def method(A: FcaSet, B: FcaSet, y: Int): Unit = {
    output(A, B)
    var j = y
    while(j < attributes) {
      if (!B.contains(j)) {
        val ret = closeConcept(A, j)
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.equalUpTo(D, j)) {
            if (submitted.get() > cores * 2) {
              method(C, D, j + 1)
            } else {
              submitted.incrementAndGet()
              pool.submit(new Runnable {
                override def run(): Unit = {
                  submitted.decrementAndGet()
                  method(C, D, j + 1)
                }
              })
            }
          }
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
    pool.awaitQuiescence(1000, TimeUnit.DAYS)
  }
}
