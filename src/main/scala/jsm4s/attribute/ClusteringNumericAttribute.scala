package jsm4s.attribute

import java.util

import scala.collection.{SortedMap, mutable}

class ClusteringNumericAttribute(values: SortedMap[String, Int], props: SortedMap[String, Set[String]], offset: Int)
extends Attribute {


  override def apply(value: String): Seq[Int] = ???

  override def size: Int = 1
}
