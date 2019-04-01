package jsm4s.attribute

import scala.collection.mutable
import java.util

// expects sorted array by numeric value, with count + labels
case class Id3Point(value: Double, counts: Map[String, Int])

class Id3Splitter(values: Array[Id3Point], minGain: Double = 0.01) {
  val range = values.map(_.value)
  val cumulative = values.scanLeft(Map.empty[String, Int]) { (acc, point) =>
    val hash = mutable.HashMap[String, Int](point.counts.toSeq :_*)
    acc.foreach { case (k, v) =>
      hash.put(k, hash.getOrElse(k, 0) + v)
    }
    hash.toMap
  }
  private val log2 = Math.log(2)
  lazy val splits = splitRecurse(0, cumulative.length)

  def splitRecurse(left: Int, right: Int): Seq[Int] = {
    if (left == right) Seq.empty
    else {
      val full = entropyForRange(left, right)._1
      val (pivot, best) = (left until right).map(i => i -> entropyOfSplit(i, left, right)).minBy(_._2)
      val gain = full - best
      if (gain < minGain) Seq(pivot)
      else splitRecurse(left, pivot - 1) ++ Seq(pivot) ++ splitRecurse(pivot + 1, right)
    }
  }

  def lowerBound(v: Double): Int = {
    val ret = util.Arrays.binarySearch(range, v)
    if (ret < 0){
      -ret - 1
    } else ret
  }

  def entropyOfSplit(pivot: Int, left: Int, right: Int): Double = {
    val (le, ls) = entropyForRange(left, pivot)
    val (re, rs)  = entropyForRange(pivot, right)
    val total = ls + rs
    (le * ls  + re * rs) / total
  }

  def entropyForRange(from: Int, until: Int) = {
    val hash = mutable.HashMap[String, Int](cumulative(until).toSeq : _*)
    cumulative(from).foreach { case (k, v) =>
      val n = hash(k) - v
      if (n == 0) hash.remove(k)
      else hash.put(k, n)
    }
    (entropy(hash.toMap), hash.values.sum.toDouble)
  }

  def entropy(items: Map[String, Int]): Double = {
    val total = items.values.sum.toDouble
    items.values.map { v =>
        val p = v / total // probability
        -p * Math.log(p) / log2
    }.sum
  }

}
