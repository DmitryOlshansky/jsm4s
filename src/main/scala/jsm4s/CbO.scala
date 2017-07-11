package jsm4s

import java.util.concurrent.{Executors, TimeUnit}

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

  override def run = {
    super.run()
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    method(A, B, 0)
    flush()
  }
}

abstract class DynSortCbO(rows: Seq[FcaSet], attrs: Int, minSupport: Int, properties: Int)
  extends Algorithm(rows, attrs, minSupport, properties) {

  def weightsOf(set: FcaSet, toVisit: Array[Int]) = {
    val weights = Array.ofDim[Int](toVisit.length)
    for (i <- set; j <- 0.until(toVisit.length)) {
      if (rows(i).contains(toVisit(j)))
        weights(j) += 1
    }
    weights
  }

  def method(A: FcaSet, B: FcaSet, visited: FcaSet, toVisit: Array[Int]): Unit = {
    if (!output(A, B)) return
    val weights = weightsOf(A, toVisit)
    val shuffled = toVisit.zip(weights).sortWith((x, y) => x._2 < y._2).map(_._1)

    for (j <- 0 until shuffled.length) {
      val y = shuffled(j)
      if (!B.contains(y)) {
        val ret = closeConcept(A, y)
        onClosure()
        if (ret._1) {
          val C = ret._2
          val D = ret._3
          if (B.equalWithMask(D, visited)) {
            visited += y
            method(C, D, visited.dup, shuffled.slice(j + 1, shuffled.length))
          }
          else onCanonicalTestFailure()
        }
      }
      visited += y
    }
  }

  override def run = {
    super.run()
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    val visited = emptyIntent
    method(A, B, visited, 0.until(attributes).toArray)
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

  override def run = {
    super.run()
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
    method(A, B, 0)
  }
}

abstract class WaveFrontBCbO(rows: Seq[FcaSet], attrs: Int, minSupport: Int, properties: Int, threads: Int, cutOff: Int, tid: Int)
  extends Algorithm(rows, attrs, minSupport, properties) with GenericBCbO {
  var counter = 0

  override def output(extent: FcaSet, intent: FcaSet) =
    if (recDepth >= cutOff || tid == 0) super.output(extent, intent)
    else filter(extent, postProcess(intent))

  def processQueue(value: AnyRef): Unit = {
    val x = value.asInstanceOf[(FcaSet, FcaSet, Int)]
    if (recDepth != cutOff)
      method(x._1, x._2, x._3) // main thread < cutOff or post cutOff
    else {
      // round-robin among threads, each remember their tid
      if (counter == tid)
        method(x._1, x._2, x._3)
      counter += 1
      if (counter == threads) counter = 0
    }
  }

  def fork(tid: Int): WaveFrontBCbO // creates concrete descendant type

  override def run = {
    super.run()
    val A = fullExtent
    val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection

    val pool = Executors.newFixedThreadPool(threads)
    for (t <- 0 until threads)
      pool.submit(new Runnable() {
        val algo = fork(t)

        def run = {
          algo.method(A, B, 0)
        }
      })
    pool.shutdown
    pool.awaitTermination(10, TimeUnit.DAYS)
  }
}