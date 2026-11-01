package jsm4s.spark

import jsm4s.FIMI
import jsm4s.algorithm._
import jsm4s.ds._
import jsm4s.processing.SortingProcessor

/** Only data crosses the Spark boundary. Mutable mining objects are task-local. */
case class SplitPayload(id: Int, first: Int, last: Int,
                        rows: Array[Array[Int]], properties: Array[String]) extends Serializable
case class SplitMetadata(header: String, attributes: Int, order: Array[Int]) extends Serializable
case class MiningOptions(splits: Int, support: Int = 2,
                         dataStructure: String = "dense",
                         strategy: String = "noCounterExamples",
                         roundSteps: Long = 100000L,
                         verticalCut: Option[Int] = None,
                         candidateBatchSize: Int = 256) extends Serializable {
  require(splits > 0, "splits must be positive")
  require(support > 0, "support must be positive")
  require(roundSteps > 0, "round-steps must be positive")
  require(verticalCut.forall(_ >= 0), "vertical-cut must be nonnegative")
  require(candidateBatchSize > 0, "candidate-batch-size must be positive")
  require(dataStructure == "dense", "Only dense sets are supported by this initial Spark implementation")
  require(strategy == "noCounterExamples" || strategy == "votingMajority" ||
    strategy.matches("boundedVotingMajority:(100|[0-9]{1,2})"),
    "strategy must be noCounterExamples, votingMajority, or boundedVotingMajority:0..100")
}
case class SplitPlan(metadata: SplitMetadata, shards: Vector[SplitPayload])

object SplitPlan {
  def prepare(data: FIMI, options: MiningOptions): SplitPlan = {
    val sorted = sort(data, options)
    SplitPlan(sorted.metadata, partition(sorted.rows, sorted.properties, options.splits, data.attrs))
  }

  private[spark] case class SortedInput(metadata: SplitMetadata, rows: Array[Array[Int]],
                                        properties: Array[String])

  private[spark] def sort(data: FIMI, options: MiningOptions): SortedInput = {
    require(data.attrs > 0, "input must declare at least one attribute")
    require(options.splits <= data.attrs, "splits must not exceed the number of attributes")
    require(data.intents.size == data.props.size, "row/property count mismatch")
    require(data.intents.forall(_.forall(a => a >= 0 && a < data.attrs)),
      "input attribute is outside the declared range")
    val sorter = new SortingProcessor(data.intents, data.attrs, NullSink, new ArrayInt(data.attrs))
    val rows = data.intents.map(row => sorter.preProcess(row).toArray).toArray
    val properties = data.props.map(data.factory.decode).toArray
    SortedInput(SplitMetadata(data.header, data.attrs, sorter.order.clone()), rows, properties)
  }

  private[spark] def partition(rows: Array[Array[Int]], properties: Array[String],
                               splits: Int, until: Int): Vector[SplitPayload] =
    Vector.tabulate(splits) { id =>
      val first = (id.toLong * until / splits).toInt
      val last = ((id + 1L) * until / splits).toInt
      val selected = rows.indices.filter(i => rows(i).exists(a => a >= first && a < last))
      SplitPayload(id, first, last, selected.map(rows).toArray, selected.map(properties).toArray)
    }

  /** Sink sees original attribute IDs. Ownership is tested in sorted coordinates. */
  def mine(shard: SplitPayload, metadata: SplitMetadata, options: MiningOptions, sink: Sink): Unit = {
    if (shard.rows.length < options.support) return
    new CbO(context(shard, metadata, options, sink)).run(false)
  }

  /** Both recursive reference mining and resumable rounds use the same context. */
  private[spark] def context(shard: SplitPayload, metadata: SplitMetadata,
                            options: MiningOptions, sink: Sink): Context = {
    val int: IntentFactory = if (options.dataStructure == "dense") new BitInt(metadata.attributes)
                             else new ArrayInt(metadata.attributes)
    val (_, factory) = FIMI.parseFimiHeader(metadata.header)
    val rows = shard.rows.map(a => int.values(a.toSeq)).toVector
    val properties = shard.properties.map(factory.encode).toVector
    val decoded = new Sink {
      def apply(h: Hypothesis): Unit =
        sink(h.copy(intent = int.values(h.intent.map(metadata.order))))
      def close(): Unit = () // Caller owns the output resource.
    }
    Context.createContext(options.dataStructure, options.strategy, 1.0,
      rows, properties, metadata.attributes, options.support, new NullCollector, decoded,
      (r, p, attrs, support, stats, out, ext, intents, strategy, sampling) =>
        new Context(r, p, attrs, support, stats, out, ext, intents, strategy, sampling) {
          override def first: Int = shard.first
          override def last: Int = shard.last
          override def isValid(intent: FcaSet): Boolean = {
            // Empty intents are excluded by the original miner too.
            val it = intent.intIterator()
            if (!it.hasNext) false
            else {
              val minimum = it.next()
              minimum >= first && minimum < last
            }
          }
        })
  }
}
