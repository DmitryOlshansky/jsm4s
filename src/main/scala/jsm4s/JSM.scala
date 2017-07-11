package jsm4s

import java.io._
import java.util.Scanner

import com.github.tototoshi.csv.CSVReader

import scala.collection.immutable.SortedSet
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
	extends CbO(rows, attrs, minSupport, properties) with ArrayExt with BitInt with IdentityPreprocessor{
	def fork = new ArrayBitCbO(rows, attrs, minSupport, properties)
}

class ArrayBitDynSortCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int = 0, properties:Int = 0)
	extends DynSortCbO(rows, attrs, minSupport, properties) with ArrayExt with BitInt with IdentityPreprocessor{
	def fork = new ArrayBitDynSortCbO(rows, attrs, minSupport, properties)
}

class ArrayBitFCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int = 0, properties:Int = 0)
	extends FCbO(rows, attrs, minSupport, properties) with ArrayExt with BitInt  with IdentityPreprocessor{
	def fork = new ArrayBitFCbO(rows, attrs, minSupport, properties)
}

class ArrayBitWFBCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int, properties:Int, threads:Int, cutOff:Int, tid:Int=0)
	extends WaveFrontBCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
	with ArrayExt with BitInt  with IdentityPreprocessor{
	def fork(tid:Int) = new ArrayBitWFBCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
}

class ArrayBitWFFCbO(rows:Seq[FcaSet], attrs:Int, minSupport:Int, properties:Int, threads:Int, cutOff:Int, tid:Int=0)
	extends WaveFrontFCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
	with ArrayExt with BitInt with IdentityPreprocessor{
	def fork(tid:Int) = new ArrayBitWFFCbO(rows, attrs, minSupport, properties, threads, cutOff, tid)
}

object JSM{

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

    var lastUsed = 0
    // First map normal attributes that are not properties
    for ((k,v) <- uniqueValues if !properties.contains(k)){
      val len = if (v.size > 1) v.size - 1 else 1
      for ((item, i) <- v.zipWithIndex){
        if (i != 0) translation.put((item, k), Some(lastUsed + i - 1))
        else translation.put((item, k), None)
      }
      lastUsed += len
    }
    // Now map properties as pairs of attributes
    for ((k,v) <- uniqueValues if properties.contains(k)) {
      for ((item, i) <- v.zipWithIndex){
        translation.put((item, k), Some(lastUsed + i))
      }
      lastUsed += v.size
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

  // Only binary properties for now
  def tau(input: File, output: File, properties: Int) = {
    val rows = Source.fromFile(input).getLines().map(x => x.split(" ").map(_.toInt).toSet).toBuffer
    val attrs = rows.map(x => x.max).max + 1
    val normalAttrs = attrs - 2 * properties
    val writer = new OutputStreamWriter(new FileOutputStream(output))
    for (r <- rows) {
      val tau = r ++ (normalAttrs until attrs) // tau is all ones in properties
      writer.write(tau.toArray.sorted.mkString("", " ", "\n"))
    }
    writer.close()
  }

  // Only binary attributes for now
  def recognize(model: File, tau: File, output: File, properties: Int, debug: Boolean) = {
    val hypotheses = load(new FileInputStream(model))
    val examples = load(new FileInputStream(tau))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    val attrs = Math.max(hypotheses._2, examples._2)
    val normalAttrs = attrs - properties*2
    for (e <- examples._1) {
      val votesInFavor = Array.ofDim[Int](properties)
      val votesAgainst = Array.ofDim[Int](properties)
      val matching = hypotheses._1.filter(p => (p.until(normalAttrs) & e) == p.until(normalAttrs))
      if (debug) {
        println(e.mkString("Example:"," ", ""))
      }
      for (h <- matching){
        if (debug){
          val support = examples._1.count{ e => (e & h) == h }
          println(support.toString + h.mkString(">>"," ", ""))
        }
        for (i <- 0 until properties) {
          if (h.contains(normalAttrs + 2 * i)) votesAgainst(i) += 1
          else if (h.contains(normalAttrs + 2 * i + 1)) votesInFavor(i) += 1
        }
      }
      val e2 = e.until(normalAttrs)
      for (i <- 0 until properties) {
        if (votesInFavor(i) > votesAgainst(i))
          e2 += normalAttrs + 2 * i + 1
        else if (votesInFavor(i) < votesAgainst(i))
          e2 += normalAttrs + 2 * i
        else if (votesAgainst(i) > 0 && votesAgainst(i) > 0) {
          e2 += normalAttrs + 2 * i
          e2 += normalAttrs + 2 * i + 1
        }
      }
      out.write(e2.mkString("", " ", "\n"))
    }
    out.close()
  }

  def generate(name:String, rows:Seq[FcaSet], attrs: Int, minSupport:Int, properties:Int):Algorithm = {
		name match {
			case "cbo" => new ArrayBitCbO(rows, attrs, minSupport, properties)
			case "fcbo" => new ArrayBitFCbO(rows, attrs, minSupport, properties)
			case "dynsort-cbo" => new ArrayBitDynSortCbO(rows, attrs, minSupport, properties)
			case "wf-bcbo" =>
        new ArrayBitWFBCbO(rows, attrs, minSupport, properties, Runtime.getRuntime().availableProcessors(), 1)
			case "wf-fcbo" =>
        new ArrayBitWFFCbO(rows, attrs, minSupport, properties, Runtime.getRuntime().availableProcessors(), 1)
			case _ => throw new Exception(s"No algorithm ${name} is supported")
		}
	}

  def stats(validation: File, prediction: File, properties: Int) = {
    val validRows = load(new FileInputStream(validation))
    val predictedRows = load(new FileInputStream(prediction))
    val attrs = validRows._2
    val pairs = validRows._1.zip(predictedRows._1)
    val correct = pairs.count{ case (a, b) => a == b }
    var unknown = pairs.count { case (a, b) =>
      !(attrs - 2 * properties until attrs).map { i => b.contains(i) }.fold(false)((a, x) => a || x)
    }
    println(s"Correct predictions ratio $correct/${pairs.size}")
    println(s"Unknown ratio $unknown/${pairs.size}")
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