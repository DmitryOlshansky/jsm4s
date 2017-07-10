package jsm4s

import java.io._
import java.util.Scanner

import com.github.tototoshi.csv.CSVReader

import scala.io.Source
import scala.collection.{immutable, mutable}
import scala.util.Random

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

class ArrayBitCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int = 0, properties:Int = 0)
	extends CbO(rows, attrs, 0, properties) with ArrayExt with BitInt with SortingPreprocessor{
	def fork = new ArrayBitCbO(rows, attrs, properties)
}

class ArrayBitDynSortCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int = 0, properties:Int = 0)
	extends DynSortCbO(rows, attrs, minSupport, properties) with ArrayExt with BitInt with IdentityPreprocessor{
	def fork = new ArrayBitDynSortCbO(rows, attrs)
}

class ArrayBitFCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int = 0, properties:Int = 0)
	extends FCbO(rows, attrs, minSupport, properties) with ArrayExt with BitInt  with SortingPreprocessor{
	def fork = new ArrayBitFCbO(rows, attrs)
}

class ArrayBitWFBCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int, properties:Int, threads:Int, cutOff:Int, tid:Int=0)
	extends WaveFrontBCbO(rows, attrs, threads, minSupport, properties, cutOff, tid)
	with ArrayExt with BitInt  with SortingPreprocessor{
	def fork(tid:Int) = new ArrayBitWFBCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
}

class ArrayBitWFFCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int, properties:Int, threads:Int, cutOff:Int, tid:Int=0)
	extends WaveFrontFCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
	with ArrayExt with BitInt with SortingPreprocessor{
	def fork(tid:Int) = new ArrayBitWFFCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
}

object FIMI{
  def encode(input: InputStream, output: OutputStream, properties: List[Int]) = {
    val reader = CSVReader.open(new InputStreamReader(input))
    val uniqueValues = mutable.SortedMap[Int, Set[String]]()
    val values = mutable.Buffer[Seq[String]]()
    for(line <- reader) {
      values += line
      line.zipWithIndex.foreach(x =>
        uniqueValues.get(x._2) match {
          case Some(set) => uniqueValues.put(x._2, set +  x._1)
          case None => uniqueValues.put(x._2, Set[String](x._1))
        }
      )
    }

    // Translate value from given position to a sequence of binary attributes
    val translation = mutable.HashMap[(String, Int), Option[Int]]()
    // check which bits are set in 'i' and output them shifted by 'start'
    def binEncode(start: Int, i: Int) =
      (0 until 32).filter(b => (i & (1<<b)) != 0).map(b => start + b)

    var lastUsed = 0
    // First map normal attributes that are not properties
    for ((k,v) <- uniqueValues if !properties.contains(k)){
      val values = if (v.size > 1) v.size - 1 else 1
      for ((item, i) <- v.zipWithIndex){
        if (i != 0) translation.put((item, k), Some(lastUsed + i))
        else translation.put((item, k), None)
      }
      lastUsed += values
    }
    // Now map properties as pairs of attributes
    for ((k,v) <- uniqueValues if properties.contains(k)) {
      val values = v.size
      for ((item, i) <- v.zipWithIndex){
        translation.put((item, k), Some(lastUsed + i))
      }
      lastUsed += values
    }
    for(line <- values) {
      output.write(line.zipWithIndex.map(translation).flatten.sorted.mkString(""," ", "\n").getBytes)
    }
    output.close()
	}

  def split(input: File, first: File, second: File, firstPart: Int, secondPart: Int) = {
    val full = firstPart + secondPart
    val rnd = new Random()
    val firstWriter = new OutputStreamWriter(new FileOutputStream(first))
    val secondWriter = new OutputStreamWriter(new FileOutputStream(second))
    for(line <- Source.fromFile(input).getLines()) {
      if (rnd.nextInt(full) < firstPart) firstWriter.write(line + "\n")
      else secondWriter.write(line + "\n")
    }
    firstWriter.close()
    secondWriter.close()
  }

  def algorithm(name:String, rows:Seq[FcaSet], attrs: Int, minSupport:Int, properties:Int):Algorithm = {
		name match {
			case "cbo" => new ArrayBitCbO(rows, attrs)
			case "fcbo" => new ArrayBitFCbO(rows, attrs)
			case "dynsort-cbo" => new ArrayBitDynSortCbO(rows, attrs)
			case "wf-bcbo" => new ArrayBitWFBCbO(rows, attrs, minSupport, properties, Runtime.getRuntime().availableProcessors(), 1)
			case "wf-fcbo" => new ArrayBitWFFCbO(rows, attrs, minSupport, properties, Runtime.getRuntime().availableProcessors(), 1)
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