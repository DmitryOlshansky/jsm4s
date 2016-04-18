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
  def max: Int
}

object Algorithm{
	def supports(rows:Seq[FcaSet], attributes:Int) =
		(0 until attributes).map(a => rows.count(r => r contains a))
}

abstract class Algorithm (
	val rows:Seq[FcaSet], val attributes:Int, val supps:Seq[Int]
) extends Runnable with ExtentFactory with IntentFactory with StatsCollector{
	//
	def this(objs:Seq[FcaSet], attributes:Int) = this(objs, attributes,Algorithm.supports(objs, attributes))
	
	val objects = rows.size
	var sortAttributes = false
	var minSupport = 0

	var out: OutputStream = System.out
	val buf = new ByteArrayOutputStream()
	// filter on extent-intent pair
	var filter = (a:FcaSet,b:FcaSet)=>true // always accept hypot

	def output(extent:FcaSet, intent:FcaSet) = {
		buf.write(intent.mkString("", " ", "\n").getBytes("UTF-8"))
		if (buf.size() > 100000) {
			flush()
		}
	}

	def flush() = {
		buf.writeTo(out)
		buf.reset()
	}

	def closeConcept(A: FcaSet, y:Int) = {
		var C = emptyExtent
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
		flush()
	}
}

trait GenericAlgorithm extends Algorithm {
	def processQueue(value:AnyRef):Unit
}

trait GenericBCbO extends GenericAlgorithm{

	var recDepth = 0

	def method(A:FcaSet, B:FcaSet, y:Int):Unit = {
		val q = mutable.Queue[(FcaSet, FcaSet, Int)]()
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


abstract class TpBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int)
extends ParallelAlgorithm(rows, attrs, threads, cutOff) with GenericBCbO{
	def processQueue(value: AnyRef): Unit = {
		def fn(t:(FcaSet, FcaSet, Int)) = serial.method(t._1, t._2, t._3)
		val x = value.asInstanceOf[(FcaSet, FcaSet, Int)]
		if(recDepth <= cutOff)
			method(x._1, x._2, x._3)
		else
			pool.submit(new Runnable(){ def run = fn(x) })
	}
	def serial: CbO

  override def run = {
  	super[GenericBCbO].run
  	pool.shutdown
  	pool.awaitTermination(10, TimeUnit.DAYS)
  }
}



abstract class NoQueueBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int)
extends Algorithm(rows, attrs) with GenericBCbO{
	var counter = 0

	def processQueue(value: AnyRef): Unit = {
		val x = value.asInstanceOf[(FcaSet, FcaSet, Int)]
		if(recDepth > cutOff || (recDepth < cutOff && tid == 0))
			method(x._1, x._2, x._3) // main thread < cutOff or post cutOff
		else{
			// round-robin among threads, each remeber their tid
			if (counter == tid)
				method(x._1, x._2, x._3)
			counter += 1
			if(counter == threads) counter = 0
		}
	}

	def fork(tid:Int): NoQueueBCbO // creates concrete descendant type

	override def run = {
		val A = fullExtent
		val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection
		val pool = Executors.newFixedThreadPool(threads)
		for(t <- 0 until threads)
			pool.submit(new Runnable(){
				val algo = fork(t)
				def run = {
					algo.method(A, B, 0)
				}
			})
		pool.shutdown
		pool.awaitTermination(10, TimeUnit.DAYS)
	}
}