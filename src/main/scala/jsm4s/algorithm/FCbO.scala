package jsm4s.algorithm

import java.util.concurrent.{ForkJoinPool, TimeUnit}

import jsm4s.ds.FcaSet
import java.util.concurrent.atomic.AtomicInteger

case class ComputeEntry(
  val extent: FcaSet,
  val intent: FcaSet,
  val j: Int,
  val errors: Array[FcaSet]
)

abstract class GenericFCbO(context: Context)
  extends Algorithm(context) with QueueAlgorithm[ComputeEntry] {

  def method(A: FcaSet, B: FcaSet, y: Int, errors: Array[FcaSet]): Unit = {
    val q = Array.ofDim[ComputeEntry](attributes - y)
    var top = 0
    output(A, B)
    val nextErrors = Array.ofDim[FcaSet](attributes)
    var j = y
    while (j < attributes) {
      nextErrors(j) = errors(j) 
      if (!B.contains(j)) {
        if (errors(j).subsetOf(B, j)) {
          val ret = closeConcept(A, j)
          stats.onClosure()
          if (ret._1) {
            val C = ret._2
            val D = ret._3
            if (B.equalUpTo(D, j)) {
              q(top) = ComputeEntry(C, D, j + 1, nextErrors)
              top += 1
            }
            else {
              nextErrors(j) = D
              stats.onCanonicalTestFailure()
            }
          }
        }
      }
      j += 1
    }
    var k = 0
    while (k < top) {
      processQueue(q(k))
      k += 1
    }
  }

  override def perform = {
    val A = ext.full
    val B = rows.fold(int.full)((a, b) => a & b) // full intersection
    val implied = Array.ofDim[FcaSet](attributes)
    for (i <- 0 until attributes) implied(i) = int.empty
    method(A, B, 0, implied)
  }
}

class FCbO(context: Context)  extends GenericFCbO(context) {
  def processQueue(x: ComputeEntry): Unit = {
    method(x.extent, x.intent, x.j, x.errors)
  }
}

class PFCbO(context: Context, threads: Int) extends GenericFCbO(context)  { 
  private val pool = if (threads == 0) ForkJoinPool.commonPool else new ForkJoinPool(threads)
  private val cores = pool.getParallelism()
  private val submitted = new AtomicInteger(0)

  override def processQueue(entry: ComputeEntry) = {
    if (submitted.get() < cores * 4) {
      submitted.incrementAndGet()
      pool.submit(new Runnable {
        override def run(): Unit = {
          submitted.decrementAndGet()
          method(entry.extent, entry.intent, entry.j, entry.errors)
        }
      })
    } else {
      method(entry.extent, entry.intent, entry.j, entry.errors)
    }
  }

  override def perform = {
    super.perform()
    pool.awaitQuiescence(1000, TimeUnit.DAYS)
  }
}