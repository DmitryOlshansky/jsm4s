package jsm4s.attribute

import java.util

import scala.collection.{SortedMap, mutable}

class ClusteringNumericAttribute(values: SortedMap[String, Int], props: SortedMap[String, Set[String]], offset: Int)
extends Attribute {

  private val numValues = values.map{ pair => (pair._1.toDouble, pair._2) }

  private val clustering = {
    val nums = values.keys.map(_.toDouble).toSeq.sorted
    val num2prop = props.map {
      case (k,v) => (k.toDouble, v.mkString("-"))
    }
    divide(nums, num2prop)
  }.toArray

  private def score(pivot: Double, nums: Seq[Double], num2prop: SortedMap[Double, String]): (Double, Boolean, Boolean) = {
    val (left, right) = nums.partition(v => v < pivot)

    def differenceOf2Max(side: Seq[Double]) = {
      val counts = side.map(v => num2prop(v)).foldLeft(mutable.Map[String, Int]()) { (map, value) =>
        map.put(value, map.getOrElse(value, 0) + 1)
        map
      }
      val top2 = counts.toSeq.sortBy(pair => pair._2).takeRight(2)
      if (top2.size == 2) (top2(1)._2 - top2(0)._2, false)
      else if (top2.size == 1) (top2.head._2, true)
      else (0, true)
    }

    def elements(side: Seq[Double]) = (side.map(numValues).sum, side.size < 10)

    val leftDiff = elements(left)
    val rightDiff = elements(right)
    val score = -(leftDiff._1.toDouble * rightDiff._1.toDouble)
    (score, leftDiff._2, rightDiff._2)
  }

  private def divide(nums: Seq[Double], num2prop: SortedMap[Double, String]): Seq[Double] = {
    if (nums.size <= 2) Seq((nums.head + nums.last)/2)
    else {
      val start = nums.head
      val end = nums.last
      val N = 25
      val step = (end - start) / N
      val point = (0 until N).map { i =>
        val pivot = start + i * step
        (i, score(pivot, nums, num2prop))
      }.minBy {
        case (i, score) => score._1
      }
      if (point._2._1 == 0) Seq((nums.head + nums.last)/2)
      else {
        val pivot = start + point._1 * step
        val (left, right) = nums.partition(v => v < pivot)
        val leftDivison = if (!point._2._2) divide(left, num2prop) else Seq()
        val rightDivision = if (!point._2._3) divide(right, num2prop) else Seq()
        leftDivison ++ Seq(pivot) ++ rightDivision
      }
    }
  }

  override def apply(value: String): Seq[Int] = {
    val ret = util.Arrays.binarySearch(clustering, value.toDouble)
    if (ret < 0){
      val idx = -ret - 1
      if (idx == 0) Seq(offset)
      else Seq(offset + idx - 1, offset + idx)
    }
    else Seq(ret)
  }

  override def size: Int = clustering.size + 1
}
