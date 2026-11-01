package jsm4s.spark

import jsm4s.algorithm._
import jsm4s.FIMI
import jsm4s.ds.{BitInt, FcaSet}

/** Support is measured on all original objects in V, never summed across H shards.
  * The nested candidate is the existing local verifier request. Its sets are read-only.
  * properties is the nonempty merged property encoded with the FIMI header factory;
  * keeping it as text avoids serializing the property implementation/factory.
  */
case class SupportedVerticalCandidate(candidate: VerticalCandidate, support: Int, properties: String,
                                       requiresHorizontalCheck: Boolean = true) extends Serializable

/** Same fixed context/order required on restore, as for CbOState. IDs are scoped to
  * one V run, start at zero, and advance only when a candidate is emitted.
  */
case class VerticalState(cbo: CbOState, nextCandidateId: Long, minSupport: Int,
                         strategy: String) extends Serializable {
  def done: Boolean = cbo.done
}

/** Persist candidates and continuation together when integrating distributed rounds.
  * An empty batch is not completion: inspect state.done, not candidates.isEmpty.
  */
case class VerticalBatch(state: VerticalState, candidates: Vector[SupportedVerticalCandidate],
                         progress: CbOProgress, rejectedProperties: Long) extends Serializable

object VerticalRound {
  /** Generate supported, nonempty V-closed intents with nonempty merged properties.
    * Property rejection suppresses only output, never traversal of descendants.
    * Candidates stay in sorted global coordinates; H verification is still separate.
    * maxCandidates=0 applies backpressure without advancing an existing state.
    * Initial root construction and individual closure scans remain uninterruptible.
    */
  def advance(vertical: VerticalPayload, metadata: SplitMetadata, options: MiningOptions,
              maxCandidates: Int, state: Option[VerticalState] = None): VerticalBatch = {
    require(maxCandidates >= 0, "Candidate batch size must be nonnegative")
    require(vertical.cut >= 0 && vertical.cut <= metadata.attributes, "Invalid vertical cut")
    require(vertical.rows.length == vertical.properties.length, "Vertical row/property count mismatch")
    state.foreach { saved =>
      require(saved.minSupport == options.support, "Continuation support does not match the V miner")
      require(saved.strategy == options.strategy, "Continuation strategy does not match the V miner")
      require(saved.nextCandidateId >= 0, "Invalid next candidate ID")
    }
    val ints = new BitInt(metadata.attributes)
    val rows = vertical.rows.map { row =>
      require(row.forall(a => a >= vertical.cut && a < metadata.attributes),
        "Vertical rows must contain only globally sorted suffix attributes")
      ints.values(row.toSeq)
    }.toVector
    val (_, factory) = FIMI.parseFimiHeader(metadata.header)
    val properties = vertical.properties.map(factory.encode).toVector
    // Use the same strategy selection and Algorithm.merge as the original CLI/H miners.
    val context = Context.createContext(options.dataStructure, options.strategy, 1.0,
      rows, properties, metadata.attributes, options.support, new NullCollector, NullSink,
      (r, p, attrs, support, stats, out, ext, intents, strategy, sampling) =>
        new Context(r, p, attrs, support, stats, out, ext, intents, strategy, sampling) {
          override def first: Int = vertical.cut
        })
    val candidates = Vector.newBuilder[SupportedVerticalCandidate]
    var count = 0
    var nextId = state.map(_.nextCandidateId).getOrElse(0L)
    var rejectedProperties = 0L
    val miner = new ResumableCbO(context, state.map(_.cbo)) {
      override def output(extent: FcaSet, intent: FcaSet): Unit = {
        val support = extent.size
        if (support >= options.support && intent.nonEmpty) {
          val merged = merge(extent, intent)
          if (merged.empty) rejectedProperties += 1
          else {
            val id = nextId
            nextId = Math.addExact(nextId, 1L)
            candidates += SupportedVerticalCandidate(VerticalCandidate(id, vertical.cut, intent), support,
              factory.decode(merged), requiresHorizontalCheck = !vertical.canSkipHorizontalChecks(support))
            count += 1
          }
        }
      }
    }
    val progress = miner.advanceUntil(options.roundSteps)(count >= maxCandidates)
    VerticalBatch(VerticalState(miner.snapshot(), nextId, options.support, options.strategy),
      candidates.result(), progress, rejectedProperties)
  }
}
