package jsm4s.algorithm

import java.io.{ByteArrayOutputStream, OutputStream, OutputStreamWriter}

import com.typesafe.scalalogging.LazyLogging
import jsm4s.FIMI
import jsm4s.ds._
import jsm4s.property.{Properties, Property}

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

trait StatsCollector {
  def onClosure(): Unit

  def onCanonicalTestFailure(): Unit

  def printStats(): Unit
}

class SimpleCollector extends StatsCollector with LazyLogging {
  var closures = 0
  var canonicalTests = 0

  def onClosure(): Unit = closures += 1

  def onCanonicalTestFailure(): Unit = canonicalTests += 1

  def printStats(): Unit = {
    logger.info(s"Closures $closures")
    logger.info(s"Canonical test failures $canonicalTests")
  }
}

class NullCollector extends StatsCollector {
  def onClosure(): Unit = {}

  def onCanonicalTestFailure(): Unit = {}

  def printStats(): Unit = {}
}

trait Sink {
  def apply(hypothesis: Hypothesis):Unit

  def close():Unit
}

class StreamSink(header: String, val out: OutputStream) extends Sink {
  val writer = new OutputStreamWriter(out)

  writer.write(header + "\n")

  override def apply(h: Hypothesis) = {
    val str = h.intent.mkString(" ") + h.props.value.mkString(" | ", " ", "\n")
    writer.synchronized {
      writer.write(str)
    }
  }

  override def close(): Unit = {
    writer.close()
  }
}

abstract class Algorithm(
                          var rows: Seq[FcaSet], val props: Seq[Properties],
                          val attributes: Int, val minSupport: Int,
                          val stats: StatsCollector, val sink: Sink
                        ) extends Runnable with ExtentFactory with IntentFactory with Preprocessor {

  val emptyProperties = new Properties(Seq())

  // filter on extent-intent pair
  def merge(extent: FcaSet, intent: FcaSet): Properties = {
    if (props.size == 0) emptyProperties
    else {
      val properties = extent.map(e => props(e)).reduceLeft((a,b) => a & b)
      properties
    }
  }

  def output(extent: FcaSet, intent: FcaSet):Unit = {
    val postProcessed = this match {
      case _: IdentityPreprocessor => intent
      case _ => postProcess(intent)
    }
    val props = merge(extent, postProcessed)
    if (!props.empty && extent.size >= minSupport)
      sink(Hypothesis(postProcessed, props))
  }

  def closeConcept(A: FcaSet, y: Int) = {
    var C = emptyExtent.dup
    var D = fullIntent.dup
    var cnt = 0
    for (i <- A) {
      if (rows(i) contains y) {
        C += i
        D &= rows(i)
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
    sink.close()
  }
}

trait QueueAlgorithm extends Algorithm {
  def processQueue(value: AnyRef): Unit
}

class ArrayBitCbO(rows: Seq[FcaSet], props: Seq[Properties],
                  attrs: Int, minSupport: Int,
                  stats: StatsCollector, sink: Sink)
  extends CbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with BitInt with IdentityPreprocessor

class ArrayBitPCbO(rows: Seq[FcaSet], props: Seq[Properties],
                   attrs: Int, minSupport: Int,
                   stats: StatsCollector, sink: Sink)
  extends PCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with BitInt with IdentityPreprocessor


class ArrayBitDynSortCbO(rows: Seq[FcaSet], props: Seq[Properties],
                         attrs: Int, minSupport: Int,
                         stats: StatsCollector, sink: Sink)
  extends DynSortCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with BitInt with IdentityPreprocessor

class ArrayBitFCbO(rows: Seq[FcaSet], props: Seq[Properties],
                   attrs: Int, minSupport: Int,
                   stats: StatsCollector, sink: Sink)
  extends FCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with BitInt with IdentityPreprocessor

class ArrayBitPFCbO(rows: Seq[FcaSet], props: Seq[Properties],
                    attrs: Int, minSupport: Int,
                    stats: StatsCollector, sink: Sink)
  extends PFCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with BitInt with IdentityPreprocessor


class ArraySparseBitCbO(rows: Seq[FcaSet], props: Seq[Properties],
                  attrs: Int, minSupport: Int,
                  stats: StatsCollector, sink: Sink)
  extends CbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with SparseBitInt with IdentityPreprocessor

class ArraySparseBitPCbO(rows: Seq[FcaSet], props: Seq[Properties],
                         attrs: Int, minSupport: Int,
                         stats: StatsCollector, sink: Sink)
  extends PCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with SparseBitInt with IdentityPreprocessor

class ArraySparseBitDynSortCbO(rows: Seq[FcaSet], props: Seq[Properties],
                         attrs: Int, minSupport: Int,
                         stats: StatsCollector, sink: Sink)
  extends DynSortCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with SparseBitInt with IdentityPreprocessor

class ArraySparseBitFCbO(rows: Seq[FcaSet], props: Seq[Properties],
                   attrs: Int, minSupport: Int,
                   stats: StatsCollector, sink: Sink)
  extends FCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with SparseBitInt with IdentityPreprocessor

class ArraySparseBitPFCbO(rows: Seq[FcaSet], props: Seq[Properties],
                         attrs: Int, minSupport: Int,
                         stats: StatsCollector, sink: Sink)
  extends PFCbO(rows, props, attrs, minSupport, stats, sink) with ArrayExt with SparseBitInt with IdentityPreprocessor



object Algorithm extends LazyLogging {
  def apply(name: String, dataStructure: String, data: FIMI,
               minSupport: Int, stats:StatsCollector, sink:Sink): Algorithm = {
    val total = data.intents.foldLeft(0L){(a,b) => a + b.size }
    val density = 100*total / (data.intents.size * data.attrs).toDouble
    logger.info("Context density is {}", density)
    val algo = dataStructure match {
      case "sparse" =>
        logger.info("Using sparse data-structure")
        val sparseSets = data.intents.map(x => SparseBitSet(x))
        name match {
          case "cbo" => new ArraySparseBitCbO(sparseSets, data.props, data.attrs, minSupport, stats, sink)
          case "fcbo" => new ArraySparseBitFCbO(sparseSets, data.props, data.attrs, minSupport, stats, sink)
          case "pcbo" => new ArraySparseBitPCbO(sparseSets, data.props, data.attrs, minSupport, stats, sink)
          case "pfcbo" => new ArraySparseBitPFCbO(sparseSets, data.props, data.attrs, minSupport, stats, sink)
          case "dynsort-cbo" => new ArraySparseBitDynSortCbO(sparseSets, data.props, data.attrs, minSupport, stats, sink)
          case _ => throw new Exception(s"No algorithm ${name} is supported")
        }
      case "dense" =>
      logger.info("Using dense data-structure")
        name match {
          case "cbo" => new ArrayBitCbO(data.intents, data.props, data.attrs, minSupport, stats, sink)
          case "fcbo" => new ArrayBitFCbO(data.intents, data.props, data.attrs, minSupport, stats, sink)
          case "pcbo" => new ArrayBitPCbO(data.intents, data.props, data.attrs, minSupport, stats, sink)
          case "pfcbo" => new ArrayBitPFCbO(data.intents, data.props, data.attrs, minSupport, stats, sink)
          case "dynsort-cbo" => new ArrayBitDynSortCbO(data.intents, data.props, data.attrs, minSupport, stats, sink)
          case _ => throw new Exception(s"No algorithm ${name} is supported")
        }
    }
    logger.info("Using {} algorithm", name)
    algo
  }
}