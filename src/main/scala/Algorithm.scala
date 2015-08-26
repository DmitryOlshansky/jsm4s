package jsm4s

import java.io.OutputStream
import java.util.concurrent.{Executors, ExecutorService}

import scala.collection._

object Algorithm{
	def supports(rows:Seq[SortedSet[Int]], attributes:Int) =
		(0 until attributes).map(a => rows.count(r => r contains a))
}

abstract class Algorithm (
	val rows:Seq[SortedSet[Int]], val attributes:Int, val supps:Seq[Int]
) extends Runnable with ExtentFactory with IntentFactory with StatsCollector{
	//
	def this(objs:Seq[SortedSet[Int]], attributes:Int) = this(objs, attributes,Algorithm.supports(objs, attributes))
	
	val objects = rows.size
	var sortAttributes = false
	var minSupport = 0
	var out: OutputStream = System.out
	// filter on extent-intent pair
	var filter = (a:SortedSet[Int],b:SortedSet[Int])=>true // always accept hypot

	def output(extent:SortedSet[Int], intent:SortedSet[Int]) = 
		out.write(intent.mkString(""," ", "\n").getBytes("UTF-8"))

	def closeConcept(A: SortedSet[Int], y:Int) = {
		var C = emptyExtent
		var D = fullIntent

		var cnt = 0
		for(i <- A) {
			if(rows(i) contains y){
				C += i
				D = D.intersect(rows(i))
				cnt += 1
			}
		}
		(cnt >= minSupport, C, D)
	}

	def run():Unit 
}

trait ExtentFactory {
	val objects:Int
	def emptyExtent:SortedSet[Int]
	def fullExtent:SortedSet[Int]
}

trait IntentFactory{
	val attributes:Int
	def emptyIntent:SortedSet[Int]
	def fullIntent:SortedSet[Int]
}

trait StatsCollector {
	def onClosure():Unit = {}
	def onCanonicalTestFailure():Unit = {}
}

abstract class ParallelAlgorithm(
	rows:Seq[SortedSet[Int]], attributes:Int, supps:Seq[Int],
	val threads:Int,
	val cutOff:Int
) extends Algorithm(rows, attributes, supps) {

	def this(objs:Seq[SortedSet[Int]], attributes:Int, threads:Int, cutOff:Int) = 
		this(objs, attributes, Algorithm.supports(objs, attributes), threads, cutOff)
	
	val pool = Executors.newFixedThreadPool(threads)

}

abstract class CbO(rows:Seq[SortedSet[Int]], attrs:Int, supps:Seq[Int])
extends Algorithm(rows, attrs, supps) {

	def this(rows:Seq[SortedSet[Int]], attrs:Int) = this(rows, attrs, Algorithm.supports(rows, attrs))

	def method(A:SortedSet[Int], B:SortedSet[Int], y:Int):Unit = {
		output(A,B)
		for(j <- y until attributes) {
			if(!B.contains(j)){
				val ret = closeConcept(A, j)
				if(ret._1){
					val C = ret._2
					val D = ret._3
					if (B.until(j) == D.until(j)) method(C, D, j+1)
					else onCanonicalTestFailure()
				}
			}
		}
	}

	def run = {
		val A = fullExtent
		val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection
		method(A, B, 0)
	}
}

trait GenericAlgorithm extends Algorithm {
	def processQueue[T](value:T):Unit
}

trait GenericBCbO extends GenericAlgorithm{

	var recDepth = 0

	def method(A:SortedSet[Int], B:SortedSet[Int], y:Int):Unit = {
		val q = mutable.Queue[(SortedSet[Int], SortedSet[Int], Int)]()
		output(A,B)
		for(j <- y until attributes) {
			if(!B.contains(j)){
				val ret = closeConcept(A, j)
				if(ret._1){
					val C = ret._2
					val D = ret._3
					if (B.until(j) == D.until(j)) q.enqueue((C, D, j+1))
					else onCanonicalTestFailure()
				}
			}
		}
		recDepth +=1 
		while(!q.isEmpty) processQueue(q.dequeue())
		recDepth -=1
	}	

	def run = {
		val A = fullExtent
		val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection
		method(A, B, 0)
	}
}


abstract class TpBCbO(rows:Seq[SortedSet[Int]], attrs:Int, threads:Int, cutOff:Int)
extends ParallelAlgorithm(rows, attrs, threads, cutOff) with GenericBCbO{
	def processQueue[T](value: T): Unit = {
		def fn(t:(SortedSet[Int], SortedSet[Int], Int)) = serial.method(t._1, t._2, t._3)
		value match{
			case x:(SortedSet[Int], SortedSet[Int], Int) =>
				if(recDepth <= cutOff)
					method(x._1, x._2, x._3)
				else
					pool.submit(new Runnable(){ def run = fn(x) })
			case _ =>
		}
	}
	def serial: CbO

  override def run = {
  	super[GenericBCbO].run
  	pool.shutdown
  }
}
