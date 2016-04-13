package jsm4s

import java.util.Arrays

import scala.collection._
import scala.collection.immutable.SortedSet

trait BitExt extends ExtentFactory{
	val emptyExtent = immutable.BitSet.empty
	val fullExtent = immutable.BitSet(0.until(objects) : _*)
}

trait TreeExt extends ExtentFactory{
	implicit val ordering = Ordering[Int]
	val emptyExtent = immutable.TreeSet.empty
	val fullExtent = immutable.TreeSet(0.until(objects) : _*)
}

trait ArrayExt extends ExtentFactory{
	val emptyExtent = SortedArray.empty
	val fullExtent = new SortedArray(0.until(objects).toArray)
}

trait BitInt extends IntentFactory{
	val emptyIntent = immutable.BitSet.empty
	val fullIntent = immutable.BitSet(0.until(attributes) : _*)
}

trait TreeInt extends IntentFactory{
	implicit val ordering = Ordering[Int]
	val emptyIntent = immutable.TreeSet.empty
	val fullIntent = immutable.TreeSet(0.until(attributes) : _*)
}

class SortedArray(table:Array[Int]) extends SortedSet[Int] {
	override implicit val ordering = Ordering[Int]

	override def rangeImpl(from: Option[Int], until: Option[Int]): SortedSet[Int] = {
		(from, until) match {
			case (Some(i), Some(j)) => new SortedArray(Arrays.copyOfRange(table, i,j))
			case (None, Some(j)) => new SortedArray(Arrays.copyOfRange(table, 0, j))
			case (Some(i), None) => new SortedArray(Arrays.copyOfRange(table, i, table.size))
			case (None, None) => this
		}
	}

	override def +(elem: Int): SortedSet[Int] = {
		var point = Arrays.binarySearch(table, elem)
		if (point < 0){
			point = -point-1 // insertion point
			val newTable = Arrays.copyOf(table, table.size+1)
			if(point != table.size)
				for (i <- table.size-1 to point by -1) newTable(i + 1) = newTable(i)
			newTable(point) = elem
			new SortedArray(newTable)
		}
		else
			this
	}

	override def contains(elem: Int): Boolean = {
		Arrays.binarySearch(table, elem) >= 0
	}

	override def -(elem: Int): SortedSet[Int] = {
		val point = Arrays.binarySearch(table, elem)
		if (point >= 0) {
			val newTable = Arrays.copyOf(table, table.size-1)
			if(point != table.size - 1) {
				for (i <- point.until(table.size-1)) newTable(i) = table(i+1)
			}
			new SortedArray(newTable)
		}
		else
			this
	}

	override def iterator: Iterator[Int] = new Iterator[Int]{
		var i = 0
		override def hasNext: Boolean = i != table.size

		override def next(): Int = {
			val item = table(i)
			i += 1
			item
		}
	}

	override def keysIteratorFrom(start: Int): scala.Iterator[Int] = rangeImpl(Some(start), None).iterator
}


object SortedArray{
	val empty = new SortedArray(Array[Int]())
}

class TreeBitCbO(rows:Seq[SortedSet[Int]], attrs:Int) extends CbO(rows, attrs)
with ArrayExt with BitInt{
	def fork = new TreeBitCbO(rows, attrs)
}

class TreeBitTpBCbO(rows:Seq[SortedSet[Int]], attrs:Int, threads:Int, cutOff:Int)
extends TpBCbO(rows, attrs, threads, cutOff) with TreeExt with BitInt{
	def serial = new TreeBitCbO(rows, attrs) // note: serial version of algorithm
}

class TreeBitNQPBCbO(rows:Seq[SortedSet[Int]], attrs:Int, threads:Int, cutOff:Int, tid:Int=0)
extends NoQueueBCbO(rows, attrs, threads, cutOff, tid) with TreeExt with BitInt{
	def fork(tid:Int) = new TreeBitNQPBCbO(rows, attrs, threads, cutOff, tid)
}

object JSM{
	def apply(name:String, rows:Seq[immutable.SortedSet[Int]]) = {
		val attrs = rows.map(x => x.max).fold(0)((a,b) => Math.max(a,b)) + 1
		name match {
			case "cbo" => new TreeBitCbO(rows, attrs)
			case "tp-bcbo" => new TreeBitTpBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case "nqp-bcbo" => new TreeBitNQPBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported") 
		}
	}
}