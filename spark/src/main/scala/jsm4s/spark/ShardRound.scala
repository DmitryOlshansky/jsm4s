package jsm4s.spark

import jsm4s.FIMI
import jsm4s.algorithm._

/** State and output from one round are committed together. Fixed rows stay separate.
  * An empty stack finishes mining, not the shard's ability to answer future checks.
  */
case class ShardRoundResult(id: Int, state: CbOState, records: Vector[String],
                            progress: CbOProgress) extends Serializable

object ShardRound {
  def advance(shard: SplitPayload, metadata: SplitMetadata, options: MiningOptions,
              state: Option[CbOState]): ShardRoundResult = {
    // HybridRound.horizontal performs V checks before calling this, even if done.
    if (state.exists(_.done))
      return ShardRoundResult(shard.id, state.get, Vector.empty, CbOProgress(0, 0, 0, true))
    val (_, factory) = FIMI.parseFimiHeader(metadata.header)
    val records = Vector.newBuilder[String]
    val sink = new Sink {
      def apply(h: Hypothesis): Unit =
        records += h.intent.mkString(" ") + " | " + factory.decode(h.props)
      def close(): Unit = ()
    }
    val miner = new ResumableCbO(SplitPlan.context(shard, metadata, options, sink), state)
    val progress = miner.advance(options.roundSteps)
    ShardRoundResult(shard.id, miner.snapshot(), records.result(), progress)
  }
}
