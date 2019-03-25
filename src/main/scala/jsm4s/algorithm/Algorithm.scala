package jsm4s.algorithm

import java.io.{ByteArrayOutputStream, OutputStream, OutputStreamWriter}

import com.typesafe.scalalogging.LazyLogging
import jsm4s.FIMI
import jsm4s.ds._
import jsm4s.property.{Properties, Property}

import scala.collection.mutable

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
    values(intent.map(revMapping))
  }

  override def postProcess(intent: FcaSet): FcaSet = {
    initialize()
    values(intent.map(order))
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

class ArraySink extends Sink {
  private val buffer = mutable.ArrayBuffer[Hypothesis]()

  override def apply(h: Hypothesis) = {
    buffer.synchronized {
      buffer += h
    }
  }

  override def close(): Unit = {}

  def hypotheses:Seq[Hypothesis] = buffer
}

case class Context(rows: Seq[FcaSet],
                   props: Seq[Properties],
                   attributes: Int,
                   minSupport: Int,
                   stats: StatsCollector,
                   sink: Sink,
                   ext: ExtentFactory,
                   int: IntentFactory)

abstract class Algorithm(context: Context) {
  val rows = context.rows
  val props = context.props
  val attributes = context.attributes
  val minSupport = context.minSupport
  val stats = context.stats
  val sink = context.sink
  val ext = context.ext
  val int = context.int
  val emptyProperties = new Properties(Seq())

  // filter on extent-intent pair
  def merge(extent: FcaSet, intent: FcaSet): Properties = {
    if (props.isEmpty) emptyProperties
    else {
      val properties = extent.map(e => props(e)).reduceLeft((a,b) => a & b)
      properties
    }
  }

  def output(extent: FcaSet, intent: FcaSet):Unit = {
    val props = merge(extent, intent)
    if (!props.empty && extent.size >= minSupport)
      sink(Hypothesis(intent, props))
  }

  def closeConcept(A: FcaSet, y: Int) = {
    var C = ext.empty.dup
    var D = int.full.dup
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
    perform()
    sink.close()
  }
}

trait QueueAlgorithm[T] extends Algorithm {
  def processQueue(value: T): Unit
}

object Algorithm extends LazyLogging {
  def apply(name: String, dataStructure: String, data: FIMI,
               minSupport: Int, threads: Int, stats:StatsCollector, sink:Sink): Algorithm = {
    val total = data.intents.foldLeft(0L){(a,b) => a + b.size }
    val density = 100*total / (data.intents.size * data.attrs).toDouble
    logger.info("Context density is {}", density)
    val extFactory = new ArrayExt(data.intents.length)
    val algo = dataStructure match {
      case "sparse" =>
        val intFactory = new SparseBitInt(data.attrs)
        logger.info("Using sparse data-structure")
        val sparseSets = data.intents.map(x => SparseBitSet(x))
        val context = Context(sparseSets, data.props, data.attrs, minSupport, stats, sink, extFactory, intFactory)
        name match {
          case "cbo" => new CbO(context)
          case "fcbo" => new FCbO(context)
          case "pcbo" => 
            new PCbO(context, threads)
          case "pfcbo" =>
            new PFCbO(context, threads)
          case "dynsort-cbo" =>
            new DynSortCbO(context)
          case _ => throw new Exception(s"No algorithm ${name} is supported")
        }
      case "dense" =>
        logger.info("Using dense data-structure")
        val intFactory = new BitInt(data.attrs)
        val context = Context(data.intents, data.props, data.attrs, minSupport, stats, sink, extFactory, intFactory)
        name match {
          case "cbo" => new CbO(context)
          case "fcbo" => new FCbO(context)
          case "pcbo" => new PCbO(context, threads)
          case "pfcbo" => new PFCbO(context, threads)
          case "dynsort-cbo" => new DynSortCbO(context)
          case _ => throw new Exception(s"No algorithm ${name} is supported")
        }
    }
    logger.info("Using {} algorithm", name)
    algo
  }
}