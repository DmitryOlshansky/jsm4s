package jsm4s

import scala.collection._

trait BitExt extends ExtentFactory{
	val emptyExtent = immutable.BitSet.empty
	val fullExtent = immutable.BitSet(0.until(objects) : _*)
}

trait TreeExt extends ExtentFactory{
	val emptyExtent = immutable.TreeSet.empty
	val fullExtent = immutable.TreeSet(0.until(objects) : _*)
}

trait BitInt extends IntentFactory{
	val emptyIntent = immutable.BitSet.empty
	val fullIntent = immutable.BitSet(0.until(attributes) : _*)
}

trait TreeInt extends IntentFactory{
	val emptyIntent = immutable.TreeSet.empty
	val fullIntent = immutable.TreeSet(0.until(attributes) : _*)
}

class TreeBitCbO(rows:Seq[SortedSet[Int]], attrs:Int) extends CbO(rows, attrs)
with TreeExt with BitInt{
	def fork = new TreeBitCbO(rows, attrs)
}

class TreeBitTpBCbO(rows:Seq[SortedSet[Int]], attrs:Int, threads:Int, cutOff:Int)
extends TpBCbO(rows, attrs, threads, cutOff) with TreeExt with BitInt{
	def serial = new TreeBitCbO(rows, attrs) // note: serial version of algorithm
}

object JSM{
	def apply(name:String, rows:Seq[immutable.SortedSet[Int]]) = {
		val attrs = rows.map(x => x.max).fold(0)((a,b) => Math.max(a,b)) + 1
		name match {
			case "cbo" => new TreeBitCbO(rows, attrs)
			case "tp-bcbo" => new TreeBitTpBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported") 
		}
	}
}