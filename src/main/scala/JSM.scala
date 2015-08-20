package jsm4s

import scala.collection._

trait BitExt extends ExtentFactory{
	def emptyExtent = immutable.BitSet.empty
	def fullExtent(attrs:Int) = immutable.BitSet(0.until(attrs) : _*)
}

trait TreeExt extends ExtentFactory{
	def emptyExtent = immutable.TreeSet.empty
	def fullExtent(attrs:Int) = immutable.TreeSet(0.until(attrs) : _*)
}

trait BitInt extends IntentFactory{
	def emptyIntent = immutable.BitSet.empty
	def fullIntent(objs:Int) = immutable.BitSet(0.until(objs) : _*)
}

trait TreeInt extends IntentFactory{
	def emptyIntent = immutable.TreeSet.empty
	def fullIntent(attrs:Int) = immutable.TreeSet(0.until(attrs) : _*)
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