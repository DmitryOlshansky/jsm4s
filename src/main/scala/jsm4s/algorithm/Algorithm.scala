package jsm4s.algorithm

import java.io.{ByteArrayOutputStream, OutputStream}
import java.util.concurrent.Executors

import com.typesafe.scalalogging.LazyLogging
import jsm4s.ds.{ExtentFactory, FcaSet, IntentFactory}

import scala.collection._

trait Preprocessor {
  var rows: Seq[FcaSet]
  val objects = rows.size
  val attributes: Int

  def preProcess(intent: FcaSet): FcaSet

  def postProcess(intent: FcaSet): FcaSet
}

trait IdentityPreprocessor extends Preprocessor {
  override def preProcess(intent: FcaSet): FcaSet = intent

  override def postProcess(intent: FcaSet): FcaSet = intent
}

trait SortingPreprocessor extends Preprocessor with IntentFactory {
  var initialized: Boolean = false
  var order: Array[Int] = null
  var revMapping: Array[Int] = null

  def initialize() = {
    if (!initialized) {
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

trait StatsCollector extends LazyLogging {
  var closures = 0
  var canonicalTests = 0

  def onClosure(): Unit = closures += 1

  def onCanonicalTestFailure(): Unit = canonicalTests += 1

  def printStats(): Unit = {
    logger.info(s"Closures $closures")
    logger.info(s"Canonical test failures $canonicalTests")
  }
}

abstract class Algorithm(
                          var rows: Seq[FcaSet], val attributes: Int, val minSupport: Int, val properties: Int
                        ) extends Runnable with ExtentFactory with IntentFactory with StatsCollector with Preprocessor {

  var out: OutputStream = System.out
  val buf = new ByteArrayOutputStream()

  // filter on extent-intent pair
  def filter(extent: FcaSet, intent: FcaSet): Boolean = {
    if (properties == 0) true
    else {
      val attrsOnly = intent.until(attributes - 2 * properties)
      val nullAttr = attrsOnly.isEmpty
      var hasProperties = false
      for (i <- attributes - 2 * properties until attributes)
        if (intent.contains(i)) hasProperties = true
      if (hasProperties && !nullAttr && extent.size >= minSupport) {
        // Filter out by counter examples
        var failures = 0
        for (r <- rows) {
          val example = r.until(attributes - 2 * properties)
          if ((example & attrsOnly) == attrsOnly) {
            for (i <- attributes - 2 * properties until attributes)
              if (r.contains(i) ^ intent.contains(i)) failures += 1
          }
          if (failures > 0) return false
        }
        true
      }
      else
        false

    }
  }

  def output(extent: FcaSet, intent: FcaSet) = {
    val postProcessed = this match {
      case _: IdentityPreprocessor => intent
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

  def closeConcept(A: FcaSet, y: Int) = {
    var C = emptyExtent.dup
    var D = fullIntent
    var cnt = 0
    for (i <- A) {
      if (rows(i) contains y) {
        C += i
        D = D & rows(i)
        cnt += 1
      }
    }
    (cnt >= minSupport, C, D)
  }

  def perform(): Unit

  def run(): Unit = {
    rows = this match {
      case _: IdentityPreprocessor => rows
      case _ => rows.map(x => preProcess(x))
    }
    perform()
  }
}

trait GenericAlgorithm extends Algorithm {
  def processQueue(value: AnyRef): Unit
}
