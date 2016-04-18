package jsm4s


trait BitExt extends ExtentFactory{
	val emptyExtent = BitSet.empty
	val fullExtent = BitSet.full(objects)
}

trait ArrayExt extends ExtentFactory{
	val emptyExtent = SortedArray.empty
	val fullExtent = new SortedArray(0.until(objects).toArray, objects)
}

trait BitInt extends IntentFactory{
	val emptyIntent = BitSet.empty
	val fullIntent = BitSet.full(attributes)
}


class ArrayBitCbO(rows:Seq[FcaSet], attrs:Int) extends CbO(rows, attrs)
with ArrayExt with BitInt{
	def fork = new ArrayBitCbO(rows, attrs)
}

class ArrayBitTpBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int)
extends TpBCbO(rows, attrs, threads, cutOff) with ArrayExt with BitInt{
	def serial = new ArrayBitCbO(rows, attrs) // note: serial version of algorithm
}

class ArrayBitNQPBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int=0)
extends NoQueueBCbO(rows, attrs, threads, cutOff, tid) with ArrayExt with BitInt{
	def fork(tid:Int) = new ArrayBitNQPBCbO(rows, attrs, threads, cutOff, tid)
}

object JSM{
	def apply(name:String, rows:Seq[FcaSet]) = {
		val attrs = rows.map(x => x.max).fold(0)((a,b) => Math.max(a,b)) + 1
		name match {
			case "cbo" => new ArrayBitCbO(rows, attrs)
			case "tp-bcbo" => new ArrayBitTpBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case "nqp-bcbo" => new ArrayBitNQPBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported") 
		}
	}
}