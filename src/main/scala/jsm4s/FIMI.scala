package jsm4s

import java.io.InputStream
import java.util.Scanner

import scala.collection.{immutable, mutable}

trait BitExt extends ExtentFactory{
	val emptyExtent = BitSet.empty(objects)
	val fullExtent = BitSet.full(objects)
	override def newExtent(seq:Iterable[Int]) = new BitSet(seq, objects)
}

trait ArrayExt extends ExtentFactory{
	val emptyExtent = SortedArray.empty
	val fullExtent = new SortedArray(0.until(objects).toArray, objects)
	override def newExtent(seq:Iterable[Int]) = new SortedArray(seq.toArray, objects)
}

trait TreeExt extends ExtentFactory {
	val emptyExtent = TreeSet.empty
	val fullExtent = TreeSet.full(objects)
}

trait BitInt extends IntentFactory{
	val emptyIntent = BitSet.empty(attributes)
	val fullIntent = BitSet.full(attributes)
	override def newIntent(seq:Iterable[Int]) = new BitSet(seq, attributes)
}

trait TreeInt extends IntentFactory {
	val emptyIntent = TreeSet.empty
	val fullIntent = TreeSet.full(attributes)
}

class ArrayBitCbO(rows:Seq[FcaSet], attrs:Int) extends CbO(rows, attrs)
with ArrayExt with BitInt with SortingPreprocessor{
	def fork = new ArrayBitCbO(rows, attrs)
}

class ArrayBitDynSortCbO(rows:Seq[FcaSet], attrs:Int) extends DynSortCbO(rows, attrs)
	with ArrayExt with BitInt with IdentityPreprocessor{
	def fork = new ArrayBitDynSortCbO(rows, attrs)
}

class ArrayBitFCbO(rows:Seq[FcaSet], attrs:Int) extends FCbO(rows, attrs)
	with ArrayExt with BitInt  with SortingPreprocessor{
	def fork = new ArrayBitFCbO(rows, attrs)
}

class ArrayBitWFBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int=0)
extends WaveFrontBCbO(rows, attrs, threads, cutOff, tid) with ArrayExt with BitInt  with SortingPreprocessor{
	def fork(tid:Int) = new ArrayBitWFBCbO(rows, attrs, threads, cutOff, tid)
}

class ArrayBitWFFCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int=0)
extends WaveFrontFCbO(rows, attrs, threads, cutOff, tid) with ArrayExt with BitInt with SortingPreprocessor{
	def fork(tid:Int) = new ArrayBitWFFCbO(rows, attrs, threads, cutOff, tid)
}

object FIMI{
	def algorithm(name:String, rows:Seq[FcaSet], attrs: Int):Algorithm = {
		name match {
			case "cbo" => new ArrayBitCbO(rows, attrs)
			case "fcbo" => new ArrayBitFCbO(rows, attrs)
			case "dynsort-cbo" => new ArrayBitDynSortCbO(rows, attrs)
			case "wf-bcbo" => new ArrayBitWFBCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case "wf-fcbo" => new ArrayBitWFFCbO(rows, attrs, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported")
		}
	}

	def load(in: InputStream):(Seq[FcaSet], Int) = {
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
		(rows.map(x => new BitSet(x, attrs)), attrs)
	}
}