package jsm4s.spark

import jsm4s.ds.{BitInt, FcaSet}

/** IDs are assigned by the caller; sets in requests/replies must be treated as read-only. */
case class VerticalCandidate(id: Long, cut: Int, intent: FcaSet) extends Serializable
case class HorizontalCandidateCheck(candidateId: Long, shardId: Int, matchingRows: Int,
                                     omittedAttributes: FcaSet) extends Serializable
case class VerticalVerification(candidateId: Long, support: Int, closure: FcaSet,
                                omittedAttributes: FcaSet, closedInProjection: Boolean,
                                hasSupport: Boolean, nonempty: Boolean,
                                checks: Vector[HorizontalCandidateCheck]) extends Serializable {
  /** Structural FCA acceptance only; JSM property merging/filtering is a later step. */
  def accepted: Boolean = nonempty && hasSupport && closedInProjection && omittedAttributes.isEmpty
}

private[spark] object CandidateSets {
  def validate(candidate: VerticalCandidate, cut: Int, attributes: Int): Unit = {
    require(candidate.cut == cut, "Candidate cut does not match the vertical context")
    require(candidate.intent.forall(a => a >= cut && a < attributes),
      "Candidate attributes must lie in the globally sorted suffix")
  }

  // Do not use FcaSet.subsetOf: implementations require matching representations.
  def containedIn(intent: FcaSet, row: FcaSet): Boolean = {
    val it = intent.intIterator()
    while (it.hasNext) if (!row.contains(it.next())) return false
    true
  }
}

/** Reusable local H checker. Does not access or advance the shard's CbO state.
  * The payload must contain exactly the original rows incident to its owned range,
  * as produced by VerticalPlan.prepare. V provides the exact global support.
  */
class HorizontalCandidateVerifier(shard: SplitPayload, attributes: Int, cut: Int) {
  require(cut >= 0 && cut <= attributes, "Invalid vertical cut")
  require(shard.first >= 0 && shard.first <= shard.last && shard.last <= cut,
    "Horizontal ownership must lie below the vertical cut")
  private val ints = new BitInt(attributes)
  private val rows = shard.rows.map(r => ints.values(r.toSeq))

  def check(candidate: VerticalCandidate, globalSupport: Int): HorizontalCandidateCheck = {
    CandidateSets.validate(candidate, cut, attributes)
    require(globalSupport >= 0, "Candidate support must be nonnegative")
    val common = ints.values(shard.first until shard.last)
    var matches = 0
    rows.foreach { row =>
      if (CandidateSets.containedIn(candidate.intent, row)) {
        matches += 1
        common &= row
      }
    }
    require(matches <= globalSupport, "Shard support exceeds V support: inconsistent candidate/context")
    // If even one supporting object is absent from H, none of its owned attributes
    // can occur in ALL supporting objects (otherwise that object would be in H).
    val omitted = if (matches == globalSupport) common else ints.empty
    HorizontalCandidateCheck(candidate.id, shard.id, matches, omitted)
  }
}

/** Local reference implementation, deliberately not wired into Spark rounds yet.
  * V supplies exact support and suffix closure; H checks supply prefix closure.
  * H payloads overlap, so their support counts must NEVER be added together.
  * Fixed rows are decoded once and reused across candidate checks.
  */
class LocalVerticalVerifier(plan: VerticalPlan, minSupport: Int) {
  require(minSupport > 0, "Support must be positive")
  private val attributes = plan.metadata.attributes
  private val cut = plan.vertical.cut
  require(cut >= 0 && cut <= attributes, "Invalid vertical cut")
  // Every omitted attribute must have exactly one checker, including empty ranges.
  private var end = 0
  plan.horizontal.foreach { shard =>
    require(shard.first == end && shard.last >= shard.first && shard.last <= cut,
      "Horizontal ranges must cover the prefix exactly once, in order")
    end = shard.last
  }
  require(end == cut && plan.horizontal.map(_.id).distinct.size == plan.horizontal.size,
    "Horizontal checks require complete prefix coverage and unique shard IDs")
  private val ints = new BitInt(attributes)
  private val rows = plan.vertical.rows.map { row =>
    require(row.forall(a => a >= cut && a < attributes), "Invalid projected attribute")
    ints.values(row.toSeq)
  }
  private lazy val checkers = plan.horizontal.map(new HorizontalCandidateVerifier(_, attributes, cut))

  def verify(candidate: VerticalCandidate): VerticalVerification = {
    CandidateSets.validate(candidate, cut, attributes)
    val suffixClosure = ints.values(cut until attributes)
    var support = 0
    rows.foreach { row =>
      if (CandidateSets.containedIn(candidate.intent, row)) {
        support += 1
        suffixClosure &= row
      }
    }
    val checks = if (plan.vertical.canSkipHorizontalChecks(support)) Vector.empty[HorizontalCandidateCheck]
                 else checkers.map(_.check(candidate, support))
    val omitted = ints.empty.dup
    checks.foreach { check =>
      val it = check.omittedAttributes.intIterator()
      while (it.hasNext) omitted += it.next()
    }
    // Fresh result sets; do not mutate candidates or fixed context rows.
    val closure = suffixClosure.dup
    val it = omitted.intIterator()
    while (it.hasNext) closure += it.next()
    val closedInProjection = suffixClosure.size == candidate.intent.size &&
      CandidateSets.containedIn(candidate.intent, suffixClosure)
    VerticalVerification(candidate.id, support, closure, omitted, closedInProjection,
      support >= minSupport, candidate.intent.nonEmpty, checks)
  }
}
