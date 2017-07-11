package jsm4s

import java.io.{ByteArrayOutputStream, OutputStream}
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}

import com.typesafe.scalalogging.LazyLogging

import scala.collection._

/// A minimal integer set for FCA computations
trait FcaSet extends Iterable[Int]{
	def contains(x:Int):Boolean
	def +=(x:Int): FcaSet
	def &(set: FcaSet): FcaSet
	def until(j:Int):FcaSet
	def dup:FcaSet
	def ==(that:FcaSet):Boolean
	def equalWithMask(that: FcaSet, mask:FcaSet):Boolean = {
		(this & mask) == (that & mask)
	}
  def subsetOf(that:FcaSet, j:Int):Boolean
  def size: Int
}

trait Preprocessor {
	var rows: Seq[FcaSet]
	val objects = rows.size
	val attributes:Int
	def preProcess(intent: FcaSet): FcaSet
	def postProcess(intent: FcaSet): FcaSet
}

trait IdentityPreprocessor extends Preprocessor {
	override def preProcess(intent: FcaSet): FcaSet = intent
	override def postProcess(intent: FcaSet): FcaSet = intent
}

trait SortingPreprocessor extends Preprocessor with IntentFactory {
	var initialized: Boolean = false
	var order:Array[Int] = null
	var revMapping:Array[Int] = null

	def initialize() = {
		if(!initialized) {
			val weights = Array.ofDim[Int](attributes)
			for (r <- rows; e <- r) {
				weights(e) += 1
			}
			order = weights.zipWithIndex.sortBy(_._1).map(_._2)
			revMapping = Array.ofDim[Int](attributes)
			for (i <- 0 until attributes) revMapping(order(i)) = i
			initialized = true
		}
	}


	override def preProcess(intent: FcaSet): FcaSet = {
		initialize()
		newIntent(intent.map(revMapping))
	}

	override def postProcess(intent: FcaSet): FcaSet = {
		initialize()
		newIntent(intent.map(order))
	}
}

trait ExtentFactory {
	val objects:Int
	def emptyExtent:FcaSet
	def fullExtent:FcaSet
	def newExtent(x: Iterable[Int]):FcaSet
}

trait IntentFactory{
	val attributes:Int
	def emptyIntent:FcaSet
	def fullIntent:FcaSet
	def newIntent(x: Iterable[Int]):FcaSet
}

trait StatsCollector extends LazyLogging {
	var closures = 0
	var canonicalTests = 0
	def onClosure():Unit = closures += 1
	def onCanonicalTestFailure():Unit = canonicalTests +=1
	def printStats():Unit = {
		logger.info(s"Closures $closures")
		logger.info(s"Canonical test failures $canonicalTests")
	}
}

abstract class Algorithm (
	var rows:Seq[FcaSet], val attributes:Int, val minSupport:Int, val properties:Int
) extends Runnable with ExtentFactory with IntentFactory with StatsCollector with Preprocessor{

	rows = this match {
		case _:IdentityPreprocessor => rows
		case _ => rows.map(x => preProcess(x))
	}

	var out: OutputStream = System.out
	val buf = new ByteArrayOutputStream()

	// filter on extent-intent pair
	def filter(extent:FcaSet, intent:FcaSet): Boolean = {
		if (properties == 0) true
		else {
			val attrsOnly = intent.until(attributes - 2*properties)
			val nullAttr = attrsOnly.isEmpty
			var hasProperties = false
			for (i <- attributes - 2*properties until attributes)
				if (intent.contains(i)) hasProperties = true
      if (hasProperties && !nullAttr && extent.size >= minSupport) {
        // Filter out by counter examples
				/*var failures = 0
        for (r <- rows) {
          val example = r.until(attributes - 2 * properties)
          if ((example & attrsOnly) == attrsOnly) {
            for (i <- attributes - 2 * properties until attributes)
              if (r.contains(i) ^ intent.contains(i)) failures += 1
          }
          if (failures > 0) return false
        }*/
        true
      }
      else
        false

		}
	}

	def output(extent:FcaSet, intent:FcaSet) = {
		val postProcessed = this match {
			case _:IdentityPreprocessor => intent
			case _ => postProcess(intent)
		}
		if (filter(extent, postProcessed)) {
			buf.write(postProcessed.mkString("", " ", "\n").getBytes("UTF-8"))
			if (buf.size() > 16384) {
				flush()
			}
		}
		true
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

abstract class ParallelAlgorithm(
	rows:Seq[FcaSet], attributes:Int, minSupport:Int, properties:Int,
	val threads:Int, val cutOff:Int
) extends Algorithm(rows, attributes, minSupport, properties) {
	val pool = Executors.newFixedThreadPool(threads)
}

trait GenericAlgorithm extends Algorithm {
	def processQueue(value:AnyRef):Unit
}
