package jsm4s.spark

import jsm4s.algorithm.CbOState

/** All continuation, pending candidates, decisions and output for a round share
  * one committed object-file dataset. No external task-side writes are used.
  */
sealed trait HybridRoundRecord extends Serializable {
  def records: Vector[String]
}
case class CandidateVerdict(candidateId: Long, shardId: Int, rejected: Boolean) extends Serializable
case class HorizontalCommit(mining: ShardRoundResult, checks: Vector[CandidateVerdict]) extends HybridRoundRecord {
  def records: Vector[String] = mining.records
}
case class VerticalCommit(batch: VerticalBatch, records: Vector[String]) extends HybridRoundRecord
case class VerificationCommit(candidateId: Long, accepted: Boolean, records: Vector[String]) extends HybridRoundRecord

case class VerificationTally(responses: Long, rejected: Boolean) extends Serializable {
  def +(other: VerificationTally): VerificationTally =
    VerificationTally(responses + other.responses, rejected || other.rejected)
}

object HybridRound {
  /** Verification remains active even after horizontal mining has finished. */
  def horizontal(shard: SplitPayload, metadata: SplitMetadata, options: MiningOptions,
                 state: Option[CbOState], cut: Int,
                 pending: Vector[SupportedVerticalCandidate]): HorizontalCommit = {
    val checks = if (pending.isEmpty) Vector.empty[CandidateVerdict] else {
      val checker = new HorizontalCandidateVerifier(shard, metadata.attributes, cut)
      pending.map { item =>
        require(item.requiresHorizontalCheck, "Shortcut candidates must not be routed to H")
        val result = checker.check(item.candidate, item.support)
        CandidateVerdict(item.candidate.id, shard.id, result.omittedAttributes.nonEmpty)
      }
    }
    HorizontalCommit(ShardRound.advance(shard, metadata, options, state), checks)
  }

  def modelRecord(item: SupportedVerticalCandidate, metadata: SplitMetadata): String =
    item.candidate.intent.map(metadata.order).toSeq.sorted.mkString(" ") + " | " + item.properties

  def vertical(payload: VerticalPayload, metadata: SplitMetadata, options: MiningOptions,
               state: Option[VerticalState]): VerticalCommit = {
    val batch = VerticalRound.advance(payload, metadata, options, options.candidateBatchSize, state)
    val direct = batch.candidates.filterNot(_.requiresHorizontalCheck).map(modelRecord(_, metadata))
    VerticalCommit(batch, direct)
  }

  /** A missing reply must fail the round, not silently accept or drop a candidate. */
  def decide(item: SupportedVerticalCandidate, tally: Option[VerificationTally],
             expectedShards: Int, metadata: SplitMetadata): VerificationCommit = {
    require(item.requiresHorizontalCheck, "Shortcut candidates must not be verified again")
    require(tally.exists(_.responses == expectedShards.toLong),
      s"Candidate ${item.candidate.id} did not receive exactly $expectedShards H replies")
    val accepted = !tally.get.rejected
    VerificationCommit(item.candidate.id, accepted,
      if (accepted) Vector(modelRecord(item, metadata)) else Vector.empty)
  }
}

case class HybridSummary(activeH: Long = 0, activeV: Long = 0, pending: Long = 0,
                         steps: Long = 0, closures: Long = 0, hypotheses: Long = 0,
                         candidates: Long = 0, checks: Long = 0, verified: Long = 0,
                         rejected: Long = 0, shortcuts: Long = 0, rejectedProperties: Long = 0)
  extends Serializable {
  def +(b: HybridSummary): HybridSummary = HybridSummary(activeH + b.activeH, activeV + b.activeV,
    pending + b.pending, steps + b.steps, closures + b.closures, hypotheses + b.hypotheses,
    candidates + b.candidates, checks + b.checks, verified + b.verified, rejected + b.rejected,
    shortcuts + b.shortcuts, rejectedProperties + b.rejectedProperties)
  def finished: Boolean = activeH == 0 && activeV == 0 && pending == 0
}

object HybridSummary {
  def apply(record: HybridRoundRecord): HybridSummary = record match {
    case HorizontalCommit(mining, checks) => HybridSummary(
      activeH = if (mining.state.done) 0 else 1, steps = mining.progress.steps,
      closures = mining.progress.closures, hypotheses = mining.records.size, checks = checks.size)
    case VerticalCommit(batch, records) => HybridSummary(
      activeV = if (batch.state.done) 0 else 1,
      pending = batch.candidates.count(_.requiresHorizontalCheck), steps = batch.progress.steps,
      closures = batch.progress.closures, hypotheses = records.size, candidates = batch.candidates.size,
      shortcuts = records.size, rejectedProperties = batch.rejectedProperties)
    case VerificationCommit(_, accepted, records) => HybridSummary(
      hypotheses = records.size, verified = 1, rejected = if (accepted) 0 else 1)
  }
}
