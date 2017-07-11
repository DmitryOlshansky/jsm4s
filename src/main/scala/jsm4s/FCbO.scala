package jsm4s

import java.util.concurrent.{TimeUnit, Executors}

import scala.collection.{mutable, Seq}


abstract class GenericFCbO(
                            rows: Seq[FcaSet], attributes: Int, minSupport: Int, properties: Int
                          ) extends Algorithm(rows, attributes, minSupport, properties) with GenericAlgorithm {

  var recDepth = 0

  def method(A: FcaSet, B: FcaSet, y: Int, errors: Array[FcaSet], N: Int): Unit = {
    val q = Array.ofDim[(FcaSet, FcaSet, Int, Array[FcaSet], Int)](attributes - y)
    var top = 0
    if (!output(A, B)) return
    val M = N + attributes
    var j = y
    while (j < attributes) {
      errors(M + j) = errors(N + j)
      if (!B.contains(j)) {
        if (errors(N + j).subsetOf(B, j)) {
          val ret = closeConcept(A, j)
          onClosure()
          if (ret._1) {
            val C = ret._2
            val D = ret._3
            if (B.until(j) == D.until(j)) {
              q(top) = ((C, D, j + 1, errors, M))
              top += 1
            }
            else {
              errors(M + j) = D
              onCanonicalTestFailure()
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

  override def run = {
    super.run()
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    val implied = Array.ofDim[FcaSet]((attributes + 1) * attributes)
    for (i <- 0 until ((attributes + 1) * attributes)) implied(i) = emptyIntent
    method(A, B, 0, implied, 0)
  }
}

abstract class FCbO(rows: Seq[FcaSet], attrs: Int, minSupport: Int, properties: Int)
  extends GenericFCbO(rows, attrs, minSupport, properties) {
  def processQueue(value: AnyRef): Unit = {
    val x = value.asInstanceOf[(FcaSet, FcaSet, Int, Array[FcaSet], Int)]
    method(x._1, x._2, x._3, x._4, x._5)
  }
}

abstract class WaveFrontFCbO(rows: Seq[FcaSet], attrs: Int, minSupport: Int, properties: Int, threads: Int, cutOff: Int, tid: Int)
  extends GenericFCbO(rows, attrs, minSupport, properties) {
  var counter = 0

  override def output(extent: FcaSet, intent: FcaSet) =
    if (recDepth >= cutOff || tid == 0) super.output(extent, intent)
    else filter(extent, postProcess(intent))

  def processQueue(value: AnyRef): Unit = {
    val x = value.asInstanceOf[(FcaSet, FcaSet, Int, Array[FcaSet], Int)]
    if (recDepth != cutOff)
      method(x._1, x._2, x._3, x._4, x._5) // main thread < cutOff or post cutOff
    else {
      // round-robin among threads, each remember their tid
      if (counter == tid)
        method(x._1, x._2, x._3, x._4, x._5)
      counter += 1
      if (counter == threads) counter = 0
    }
  }

  def fork(tid: Int): WaveFrontFCbO // creates concrete descendant type

  override def run = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    val pool = Executors.newFixedThreadPool(threads)
    for (t <- 0 until threads)
      pool.submit(new Runnable() {
        val implied = Array.ofDim[FcaSet]((attributes + 1) * attributes)
        for (i <- 0 until ((attributes + 1) * attributes)) implied(i) = emptyIntent
        val algo = fork(t)

        def run = {
          algo.method(A, B, 0, implied, 0)
        }
      })
    pool.shutdown
    pool.awaitTermination(10, TimeUnit.DAYS)
  }
}