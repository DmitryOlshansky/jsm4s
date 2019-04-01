package jsm4s.algorithm

import jsm4s.ds.FcaSet
import jsm4s.property.Composite

class DynSortCbO(context: Context) extends Algorithm(context) {

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
      val A = ext.full
      val B = rows.fold(int.full)((a, b) => a & b) // full intersection
      val visited = int.empty
      method(A, B, visited, 0.until(attributes).toArray)
    }
  }
