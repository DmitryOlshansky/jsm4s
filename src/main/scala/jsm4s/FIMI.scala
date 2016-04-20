package jsm4s

import java.io.InputStream
import java.util.Scanner

import scala.collection.{immutable, mutable}


trait BitExt extends ExtentFactory{
	val emptyExtent = ExtentBitSet.empty
	val fullExtent = ExtentBitSet.full
}

trait ArrayExt extends ExtentFactory{
	val emptyExtent = SortedArray.empty
	val fullExtent = new SortedArray(0.until(objects).toArray, objects)
}

trait TreeExt extends ExtentFactory {
	val emptyExtent = TreeSet.empty
	val fullExtent = TreeSet.full(objects)
}

trait BitInt extends IntentFactory{
	val emptyIntent = IntentBitSet.empty
	val fullIntent = IntentBitSet.full
}

trait TreeInt extends IntentFactory {
	val emptyIntent = TreeSet.empty
	val fullIntent = TreeSet.full(attributes)
}

class ArrayBitCbO(rows:Seq[FcaSet], attrs:Int) extends CbO(rows, attrs)
with ArrayExt with BitInt{
	def fork = new ArrayBitCbO(rows, attrs)
}

class ArrayBitFCbO(rows:Seq[FcaSet], attrs:Int) extends FCbO(rows, attrs)
	with ArrayExt with BitInt{
	def fork = new ArrayBitFCbO(rows, attrs)
}

class ArrayBitTpBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int)
extends TpBCbO(rows, attrs, threads, cutOff) with ArrayExt with BitInt{
	def serial = new ArrayBitCbO(rows, attrs) // note: serial version of algorithm
}

class ArrayBitNQPBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int=0)
extends NoQueueBCbO(rows, attrs, threads, cutOff, tid) with ArrayExt with BitInt{
	def fork(tid:Int) = new ArrayBitNQPBCbO(rows, attrs, threads, cutOff, tid)
}

object FIMI{
	def algorithm(name:String, rows:Seq[FcaSet]):Algorithm = {
		val attrs = FcaSet.attributes
		name match {
			case "cbo" => new ArrayBitCbO(rows, attrs)
			case "fcbo" => new ArrayBitFCbO(rows, attrs)
			case "tp-bcbo" => new ArrayBitTpBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case "nqp-bcbo" => new ArrayBitNQPBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported")
		}
	}

	def load(in: InputStream) = {
		val scanner = new Scanner(in)
		var attrs = 0
		var rows = mutable.ArrayBuffer[immutable.SortedSet[Int]]()
		while (scanner.hasNext()) {
			val line = scanner.nextLine
			val inner = new Scanner(line)
			inner.useDelimiter("\\s+")
			var set = immutable.SortedSet[Int]()
			while (inner.hasNext) {
				val v = inner.nextInt()
				set += v
				if (v + 1 > attrs)
					attrs = v + 1
			}
			rows += set
		}
		FcaSet.attributes = attrs
		FcaSet.objects = rows.size
		rows.map(x => new IntentBitSet(x)).toSeq
	}
}