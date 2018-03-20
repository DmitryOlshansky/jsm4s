package jsm4s.attribute

import jsm4s.JsmException

import scala.collection.{SortedMap, mutable}

class BagOfWords(val freqDict: SortedMap[String, Int], val offset:Int) extends Encoder {
  private val scans = freqDict.scanLeft((0, "", 0)) {
    case (agg, (k,v)) => (agg._1 + v, k, agg._1)
  }

  private val mapping = scans.map(row => (row._2, row._3)).toMap

  override def apply(raw: Any) = raw match {
    case value: Seq[String] =>
      val counts = mutable.HashMap[String, Int]()
      value.foreach {
        case k => counts.put(k, counts.getOrElse(k, 0) +1)
      }
      counts.flatMap {
        case (k, v) =>
          val start = offset + mapping(k)
          start.until(start + v)
      }.toSeq.sorted
    case _ => throw JsmException("Unsupported value type for BagOfWords encoder")
  }

  override val size = scans.last._1

  override def toString = s"$offset:${freqDict.mkString}"
}