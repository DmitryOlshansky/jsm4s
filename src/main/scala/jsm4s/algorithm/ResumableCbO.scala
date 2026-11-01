package jsm4s.algorithm

import jsm4s.ds.FcaSet
import scala.collection.mutable.ArrayBuffer

/** Shared search/wire representation. Treat the sets as read-only after construction.
  * Frames are ordered from root to stack top.
  */
case class CbOFrame(extent: FcaSet, intent: FcaSet,
                    nextAttribute: Int, emitPending: Boolean) extends Serializable
case class CbOState(attributes: Int, objects: Int, first: Int,
                    frames: Vector[CbOFrame]) extends Serializable {
  def done: Boolean = frames.isEmpty
}
case class CbOProgress(steps: Long, closures: Long, canonicalFailures: Long,
                       done: Boolean) extends Serializable

/** Sequential DFS with explicit yield points. Restore only against the same fixed
  * context, row ordering, attribute ordering, and mining options. The sink is owned
  * by the caller: advance never closes it. A step emits, visits an attribute, or pops.
  * Root initialization and an individual closure scan are not interruptible.
  * Context, callbacks, and snapshot consumers must not mutate shared sets.
  */
class ResumableCbO(context: Context, saved: Option[CbOState] = None) extends Algorithm(context) {
  private val stack = ArrayBuffer.empty[CbOFrame]
  saved match {
    case None =>
      stack += CbOFrame(ext.full, rows.fold(int.full)((a, b) => a & b), context.first, true)
    case Some(state) =>
      require(state.attributes == attributes && state.objects == rows.size && state.first == context.first,
        "Continuation dimensions/search origin do not match the context")
      state.frames.foreach { frame =>
        require(frame.extent.getClass == ext.empty.getClass && frame.intent.getClass == int.empty.getClass,
          "Continuation set representations do not match the context")
        require(frame.nextAttribute >= context.first && frame.nextAttribute <= attributes,
          "Invalid continuation attribute cursor")
        require(frame.extent.forall(i => i >= 0 && i < rows.size) &&
          frame.intent.forall(i => i >= 0 && i < attributes), "Invalid continuation set index")
        stack += frame
      }
  }

  def done: Boolean = stack.isEmpty

  def snapshot(): CbOState = CbOState(attributes, rows.size, context.first,
    stack.toVector)

  def advance(maxSteps: Long): CbOProgress = advanceUntil(maxSteps)(false)

  /** Stop between transitions when the caller's batch/backpressure limit is met.
    * The predicate must not mutate the miner or shared sets. A closure scan is
    * still atomic; pending output remains on the stack if we stop before emitting.
    */
  def advanceUntil(maxSteps: Long)(shouldStop: => Boolean): CbOProgress = {
    require(maxSteps >= 0, "maxSteps must be nonnegative")
    var steps = 0L
    var closures = 0L
    var failures = 0L
    while (steps < maxSteps && stack.nonEmpty && !shouldStop) {
      val frame = stack.last
      if (frame.emitPending) {
        output(frame.extent, frame.intent)
        stack(stack.size - 1) = frame.copy(emitPending = false)
      } else if (frame.nextAttribute == attributes) {
        stack.remove(stack.size - 1)
      } else {
        val j = frame.nextAttribute
        // Replace the cursor, preserving frames referenced by earlier snapshots.
        stack(stack.size - 1) = frame.copy(nextAttribute = j + 1)
        if (!frame.intent.contains(j)) {
          val closure = closeConcept(frame.extent, j)
          closures += 1
          stats.onClosure()
          if (closure.hasSupport) {
            if (frame.intent.equalUpTo(closure.intent, j))
              stack += CbOFrame(closure.extent, closure.intent, j + 1, true)
            else {
              failures += 1
              stats.onCanonicalTestFailure()
            }
          }
        }
      }
      steps += 1
    }
    CbOProgress(steps, closures, failures, done)
  }

  override def perform(): Unit = { while (!done) advance(Long.MaxValue) }
}
