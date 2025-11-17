package jsm4s.algorithm

import java.io.{ByteArrayOutputStream, OutputStream, OutputStreamWriter}

import com.typesafe.scalalogging.LazyLogging
import jsm4s.FIMI
import jsm4s.algorithm.Strategies._
import jsm4s.ds._
import jsm4s.processing.SortingProcessor
import jsm4s.property.{Composite, Property, PropertyFactory}

import scala.collection.mutable

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

class StreamSink(header: String, factory: PropertyFactory, out: OutputStream) extends Sink {
  val writer = new OutputStreamWriter(out)

  writer.write(header + "\n")

  override def apply(h: Hypothesis) = {
    val str = h.intent.mkString(" ") + " | " + factory.decode(h.props) + "\n"
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
                   props: Seq[Property],
                   attributes: Int,
                   minSupport: Int,
                   stats: StatsCollector,
                   sink: Sink,
                   ext: ExtentFactory,
                   int: IntentFactory,
                   strategy: MergeStrategy)

object Context {
  def sorted(rows: Seq[FcaSet],
             props: Seq[Property],
             attributes: Int,
             minSupport: Int,
             stats: StatsCollector,
             sink: Sink,
             ext: ExtentFactory,
             int: IntentFactory,
             strategy: MergeStrategy): Context = {
    val proc = new SortingProcessor(rows, attributes, sink, int)
    val sorted = rows.map(intent => int.values(proc.preProcess(intent)))
    Context(sorted, props, attributes, minSupport, stats, proc, ext, int, strategy)
  }
}

abstract class Algorithm(context: Context) {
  val rows = context.rows
  val props = context.props
  val attributes = context.attributes
  val minSupport = context.minSupport
  val stats = context.stats
  val sink = context.sink
  val ext = context.ext
  val int = context.int
  val strategy = context.strategy
  val emptyProperties = new Composite(Seq())

  // filter on extent-intent pair
  def merge(extent: FcaSet, intent: FcaSet): Property = {
    if (props.isEmpty) emptyProperties
    else {
      val properties = strategy(extent.map(e => props(e)).toSeq)
      properties
    }
  }

  def output(extent: FcaSet, intent: FcaSet):Unit = {
    if (extent.size >= minSupport) {
      val props = merge(extent, intent)
      if (!props.empty)
        sink(Hypothesis(intent, props))
    }
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

  def run(closeSink: Boolean = true): Unit = {
    perform()
    if (closeSink) sink.close()
  }
}

trait QueueAlgorithm[T] extends Algorithm {
  def processQueue(value: T): Unit
}

object Algorithm extends LazyLogging {
  def apply(name: String, dataStructure: String, strategy: String, intents: Seq[FcaSet], props: Seq[Property], attrs: Int,
               minSupport: Int, threads: Int, stats:StatsCollector, sink:Sink): Algorithm = {
    val total = intents.foldLeft(0L){(a,b) => a + b.size }
    val density = 100*total / (intents.size * attrs).toDouble
    logger.info("Context density is {}", density)
    val extFactory = new ArrayExt(intents.length)
    val strat = strategy match {
      case "noCounterExamples" => noCounterExamples _
      case "noop" => noop _
      case "votingMajority" => votingMajority _
    }
    val context = dataStructure match {
      case "sparse" =>
        val intFactory = new SparseBitInt(attrs)
        logger.info("Using sparse data-structure")
        val sparseSets = intents.map(x => SparseBitSet(x))
        Context.sorted(sparseSets, props, attrs, minSupport, stats, sink, extFactory, intFactory, strat)
      case "dense" =>
        logger.info("Using dense data-structure")
        val intFactory = new BitInt(attrs)
        Context.sorted(intents, props, attrs, minSupport, stats, sink, extFactory, intFactory, strat)
    }
    val algo = name match {
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
    logger.info("Using {} algorithm", name)
    algo
  }
}