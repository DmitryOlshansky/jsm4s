package jsm4s.algorithm

import jsm4s.ds.FcaSet
import jsm4s.property.Properties

abstract class DynSortCbO(
                           rows: Seq[FcaSet], props: Seq[Properties],
                           attrs: Int, minSupport: Int,
                           stats: StatsCollector, sink: Sink
                         )
  extends Algorithm(rows, props, attrs, minSupport, stats, sink) {

    def weightsOf(set: FcaSet, toVisit: Array[Int]) = {
      val weights = Array.ofDim[Int](toVisit.length)
      for (i <- set; j <- 0.until(toVisit.length)) {
        if (rows(i).contains(toVisit(j)))
          weights(j) += 1
      }
      weights
    }

    def method(A: FcaSet, B: FcaSet, visited: FcaSet, toVisit: Array[Int]): Unit = {
      output(A, B)
      val weights = weightsOf(A, toVisit)
      val shuffled = toVisit.zip(weights).sortWith((x, y) => x._2 < y._2).map(_._1)

      for (j <- 0 until shuffled.length) {
        val y = shuffled(j)
        if (!B.contains(y)) {
          val ret = closeConcept(A, y)
          stats.onClosure()
          if (ret._1) {
            val C = ret._2
            val D = ret._3
            if (B.equalWithMask(D, visited)) {
              visited += y
              method(C, D, visited.dup, shuffled.slice(j + 1, shuffled.length))
            }
            else stats.onCanonicalTestFailure()
          }
        }
        visited += y
      }
    }

    override def perform = {
      val A = fullExtent
      val B = rows.fold(fullIntent)((a, b) => a & b) // full intersection
      val visited = emptyIntent
      method(A, B, visited, 0.until(attributes).toArray)
    }
  }
