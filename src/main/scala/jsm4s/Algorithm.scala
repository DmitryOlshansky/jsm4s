package jsm4s

import java.io.{ByteArrayOutputStream, OutputStream}
import java.util.concurrent.{Executors, ExecutorService, TimeUnit}

import scala.collection._

/// A minimal integer set for FCA computation
trait FcaSet extends Iterable[Int]{
	def contains(x:Int):Boolean
	def +=(x:Int): FcaSet
	def &(set: FcaSet): FcaSet
	def until(j:Int):FcaSet
	def dup:FcaSet
	def ==(that:FcaSet):Boolean
  def subsetOf(that:FcaSet, j:Int):Boolean
}

object FcaSet {
  var attributes = 0
  var objects = 0
}

object Algorithm{
	def supports(rows:Seq[FcaSet], attributes:Int) =
		(0 until attributes).map(a => rows.count(r => r contains a))
}

abstract class Algorithm (
	var rows:Seq[FcaSet], val attributes:Int, val supps:Seq[Int]
) extends Runnable with ExtentFactory with IntentFactory with StatsCollector{
	//
	def this(objs:Seq[FcaSet], attributes:Int) = this(objs, attributes,Algorithm.supports(objs, attributes))
	
	val objects = rows.size
	var sortAttributes = false
	var minSupport = 0

	val weights = Array.ofDim[Int](attributes)
	for(r <- rows; e <- r){
		weights(e) += 1
	}
	val order = weights.zipWithIndex.sortBy(_._1).map(_._2)
	val revMapping = Array.ofDim[Int](attributes)
	for(i <- 0 until attributes) revMapping(order(i)) = i
	rows = rows.map(x => new BitSet(x.map(revMapping), attributes))

	var out: OutputStream = System.out
	val buf = new ByteArrayOutputStream()
	// filter on extent-intent pair
	var filter = (a:FcaSet,b:FcaSet)=>true // always accept hypot

	def output(extent:FcaSet, intent:FcaSet) = {
		buf.write(intent.map(order).mkString("", " ", "\n").getBytes("UTF-8"))
		if (buf.size() > 16384) {
			flush()
		}
	}

	def flush() = {
		buf.writeTo(out)
		buf.reset()
	}

	def closeConcept(A: FcaSet, y:Int) = {
		var C = emptyExtent.dup
		var D = fullIntent
		var cnt = 0
		for(i <- A) {
			if(rows(i) contains y){
				C += i
				D = D & rows(i)
				cnt += 1
			}
		}
		(cnt >= minSupport, C, D)
	}

	def run():Unit 
}

trait ExtentFactory {
	val objects:Int
	def emptyExtent:FcaSet
	def fullExtent:FcaSet
}

trait IntentFactory{
	val attributes:Int
	def emptyIntent:FcaSet
	def fullIntent:FcaSet
}

trait StatsCollector {
	def onClosure():Unit = {}
	def onCanonicalTestFailure():Unit = {}
}

abstract class ParallelAlgorithm(
	rows:Seq[FcaSet], attributes:Int, supps:Seq[Int],
	val threads:Int,
	val cutOff:Int
) extends Algorithm(rows, attributes, supps) {

	def this(objs:Seq[FcaSet], attributes:Int, threads:Int, cutOff:Int) = 
		this(objs, attributes, Algorithm.supports(objs, attributes), threads, cutOff)
	
	val pool = Executors.newFixedThreadPool(threads)

}

abstract class CbO(rows:Seq[FcaSet], attrs:Int, supps:Seq[Int])
extends Algorithm(rows, attrs, supps) {

	def this(rows:Seq[FcaSet], attrs:Int) = this(rows, attrs, Algorithm.supports(rows, attrs))

	def method(A:FcaSet, B:FcaSet, y:Int):Unit = {
		output(A,B)
		var j = y
		while(j < attributes) {
			if(!B.contains(j)){
				val ret = closeConcept(A, j)
				if(ret._1){
					val C = ret._2
					val D = ret._3
					if (B.until(j) == D.until(j)) method(C, D, j+1)
					else onCanonicalTestFailure()
				}
			}
			j += 1
		}
	}

	def run = {
		val A = fullExtent
		val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection
		method(A, B, 0)
		flush()
	}
}

trait GenericAlgorithm extends Algorithm {
	def processQueue(value:AnyRef):Unit
}
