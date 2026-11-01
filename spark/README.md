# Spark generate-split

This is a separate entry point. The original `jsm-cli` and its `generate` and
`generate-split` commands remain unchanged. This application runs resumable,
sequential CbO in bounded Spark rounds. No FCbO, ForkJoin pools, or messaging broker
is involved. The default remains horizontal-only. Add `--vertical-cut` to enable
the integrated H/V pipeline, including property filtering and candidate verification.

## Build

Use Java 17 and Spark **3.5.9 built for Scala 2.12**. On macOS with Homebrew Java:

```bash
export JAVA_HOME="$(brew --prefix openjdk@17)/libexec/openjdk.jdk/Contents/Home"
export SPARK_HOME="$HOME/opt/spark-3.5.9-bin-hadoop3"
export SPARK_LOCAL_IP=127.0.0.1
bash scripts/spark-build.sh
```

The assembly is `spark/target/scala-2.12/jsm4s-spark-assembly.jar`. Spark, Hadoop,
Scala runtime, and the local CLI's logging backend are not bundled. `sbt stage`
continues to produce the original CLI launchers. The root project is not an
aggregate, so ordinary `sbt test` does not start Spark; use `sbt spark/test` too.

## Local macOS run

From the repository root, use a NEW output directory:

```bash
bash scripts/spark-submit.sh \
  "file://$PWD/mushroom-training.dat" \
  "file://$PWD/mushroom-spark-run" 5 --support 5 --round-steps 100000

bash scripts/spark-collect.sh mushroom-spark-run mushroom-spark-model.dat
```

The default master is `local[2]`. To use your already-running local standalone
master and worker instead:

```bash
export JSM_SPARK_MASTER=spark://127.0.0.1:7077
export JSM_EXECUTOR_MEMORY=1g
export JSM_EXECUTOR_CORES=1
export JSM_TOTAL_CORES=4
bash scripts/spark-submit.sh \
  "file://$PWD/mushroom-training.dat" \
  "file://$PWD/mushroom-cluster-run" 5 --support 5
```

The wrapper uses client deploy mode. On multiple machines, run it on a reachable
gateway and use storage visible to every executor (HDFS or a shared filesystem).
Do not export `SPARK_LOCAL_IP=127.0.0.1` for a multi-machine deployment. The local
collector supports uncompressed local/shared POSIX files, not Hadoop URIs.

You can also call `spark-submit` directly with class
`jsm4s.spark.SparkGenerateSplit`. Required application flags are `--input`,
`--output`, and `--splits`. Optional flags:

| Flag | Default / supported values |
|---|---|
| `--support` | 2; must be positive |
| `--algorithm` | `cbo` only |
| `--data-structure` | `dense` only |
| `--strategy` | `noCounterExamples`, `votingMajority`, `boundedVotingMajority:0..100` |
| `--sampling` | 1.0 only |
| `--round-steps` | 100000; positive maximum DFS transitions per shard per round |
| `--vertical-cut` | Omitted: horizontal-only. Otherwise a sorted attribute position from 0 through the attribute count |
| `--candidate-batch-size` | 256; positive maximum V candidates per round in H/V mode |

Splits must be between 1 and the declared attribute count. Sparse sets and random
sampling are deliberately rejected in this first version. `noop` is also rejected:
the original unsplit CLI groups by property for that strategy, and the split path
does not. That requires a separate semantics decision.

## Execution and correctness

The driver reads the input, computes ONE global attribute permutation, and selects
rows intersecting each attribute range. Each payload retains complete rows and
their properties. Contexts overlap; these are not arbitrary disjoint row chunks.
All rows supporting an intent that touches a shard's range occur in that shard.
Minimum support therefore stays absolute, rather than being divided by split count.

Each nonempty intent is emitted only by the shard containing its smallest globally
sorted attribute. This prevents duplicates, including the common-root-closure
case. Attribute IDs are restored before output. Descendants can still extend past
the shard's upper boundary. The original local split implementation is not changed.

`ResumableCbO` retains a stack of extent, intent, next attribute, and pending-output
flag. `advance(N)` executes at most N transitions, where a transition emits/tests
the current concept, visits one attribute (possibly calculating a closure), or pops
an exhausted frame. The root intersection is initialized once per new search.
Neither that initialization nor a closure scan is interruptible; N is not a time
budget. `advance(0)` is a no-op. The running stack and snapshots use the same
`CbOFrame` type. Snapshot/restore shares frames and their `FcaSet` arrays without
copying or enumerating sets. Cursor changes replace a small frame object; child
closures build fresh sets, leaving earlier snapshots unchanged. All `FcaSet`
implementations are serializable. Context, callbacks, and snapshot consumers must
treat shared sets as read-only: external mutation is not isolated. Restoration
still validates set indices. Snapshots require the SAME fixed context, row and
attribute order, set representations, and mining configuration. Old vector-based
binary snapshots are incompatible with this representation. Restoring a finished snapshot stays
finished. The original recursive `CbO` is retained as a reference.

In horizontal-only mode, the driver caches fixed shard payloads separately from the continuation RDD. Each
round joins them by shard ID, rebuilds task-local mining objects, and calls
`ShardRound.advance`. Results contain a new stack, bounded output (at most N records
per shard), and counters. Those records occupy memory until the round is written;
adjust the budget if large intents make the result batches expensive.

State AND output are saved together with Spark's output commit mechanism to a new
round directory. The driver reads that committed dataset for the next round,
truncating lineage without relying on the cache surviving executor loss. Only small
aggregate counters are returned to the driver. Each round reports active miners,
steps, and emitted hypotheses. Completed shards keep their empty stacks and consume
zero mining steps in later rounds. In hybrid mode, `HybridRound.horizontal` checks
pending V candidates even on finished shards, then calls `ShardRound.advance`.

At termination, committed round outputs are combined into the existing text model
layout. There is no shared output file or task-local spool. Retries rebuild from the
previous committed state and cannot append duplicate records to a completed round.

The output contains:

```text
header.txt                 one original FIMI header
metadata.json              input SHA-256, options, permutation, shard sizes, Spark version
rounds/round-000000/part-*  binary round records: continuation + output + counters
rounds/round-000000/_SUCCESS
rounds/round-000001/...
hypotheses/part-*           headerless model records, unspecified global order
hypotheses/_SUCCESS         Spark output completion
_COMPLETE                  data and metadata are both complete
```

The collector requires both completion markers and prepends the header exactly
once. It refuses to replace a model file and does not silently remove duplicates.
A failed application can leave an incomplete output directory; use a new output
path for a rerun. Round state is durable, but automatic driver restart/resume into
an existing output directory is NOT implemented. Binary round files are internal,
version-dependent snapshots, not a long-term portable format. Keep them until the
run completes; they also duplicate model data on disk alongside the final text.
Horizontal-only metadata remains version 2; hybrid metadata is version 3 and also
records the cut, batch bound, support shortcut threshold, and candidate/check counts.

## Integrated horizontal/vertical Spark rounds

Example for the 117-attribute mushroom context (cut 100 keeps the 17 most frequent
attributes in V; this is an illustrative cut, not an automatically tuned optimum):

```bash
bash scripts/spark-submit.sh \
  "file://$PWD/mushroom-training.dat" \
  "file://$PWD/mushroom-hybrid-run" 5 \
  --support 5 --round-steps 100000 --vertical-cut 100 --candidate-batch-size 128
bash scripts/spark-collect.sh mushroom-hybrid-run mushroom-hybrid-model.dat
```

There are N H mining contexts and one V producer context. They are Spark tasks,
not permanently assigned executor processes. Fixed H payloads and V's projected
payload are cached separately from committed continuation state. V runs on an
executor, not in the driver. H owns intents whose minimum sorted attribute is below
the cut; verified V owns suffix-only intents. Descendants of H searches can still
include suffix attributes. This ownership prevents duplicate model records.

The one-batch pipeline works as follows:

1. Read the previous round's bounded pending V candidates. Broadcast those requests
   to H; no DFS stacks, full reply sets, or model records are collected by the driver.
2. H tasks check that batch and advance their own CbO searches by at most
   `--round-steps`. In the same worker stage, V advances by that step budget or the
   candidate bound, whichever comes first, producing the next batch.
3. Reduce H's compact rejection replies by candidate ID. A candidate is accepted
   only after exactly N replies; a missing reply fails the round. Support-shortcut
   candidates bypass H and produce a record directly, after V property filtering.
4. Commit H states/output/replies, V state/next batch/direct output, and verification
   decisions/output to one object-file round directory. Read that committed dataset
   for the next iteration; failed task attempts cannot append extra model records.

Every previous batch is completely checked before advancing to another round, so
pending work cannot grow across rounds. During a round, the previous batch and one
new batch may coexist (each bounded by `--candidate-batch-size`). Candidate count
is not a byte bound: large intents/properties require corresponding memory. H
verification is bounded by the batch size, separately from its mining step budget.
This is cooperative round interleaving, not time slicing within a closure/check.

Finished H miners retain their fixed data and continue checking V candidates.
Termination requires H finished, V finished, AND no pending candidates; an extra
drain round verifies V's last batch if necessary. Empty batches are not completion.
Cuts 0 and m provide all-V/all-H boundary cases. No adaptive scheduling or direct
worker queues are used. Increasing cores beyond N+1 cannot split one context's work.

Hybrid round files contain `HorizontalCommit`, `VerticalCommit`, and
`VerificationCommit` records. They are internal/version-dependent, not compatible
with the old horizontal `ShardRoundResult` reader. Final text files, header, markers,
and the collection script are unchanged. Counters distinguish produced candidates,
H checks, completed verifications, rejections, shortcuts, and property rejections.

Validated on the local Spark 3.5.9 standalone cluster with the 6,513-row mushroom
training context, 5 H shards, support 5, and the settings above: 17 rounds,
153 V candidates, 765 H checks, 112 rejected candidates, and 105,678 final
hypotheses. The collected model matched a fresh original CbO CLI run after ordinary
sorting (not deduplication). This is a correctness check, not a performance claim.

## Local vertical cut and verification

`VerticalPlan.prepare(data, options, cut)` sorts attributes by increasing frequency
using the same ordering as `SplitPlan`. It creates N horizontal payloads owning
ranges in `[0, cut)` and one vertical payload containing attributes `[cut, m)`.
The cut is a position in this sorted order, not an original attribute ID.
`metadata.order(sortedId)` maps back to original IDs. V retains global sorted IDs,
all original rows (including empty projections and duplicates), and their aligned
properties. H retains full attributes in its selected rows. Empty H ranges are
allowed when the prefix is narrower than the shard count. Cuts 0 and m support
the all-V and all-H boundary cases.

Example local API (no SparkContext needed):

```scala
val plan = VerticalPlan.prepare(data, MiningOptions(splits = 5, support = 5), cut)
val verifier = new LocalVerticalVerifier(plan, minSupport = 5)
// sortedIntent contains only global sorted attribute IDs >= cut.
val result = verifier.verify(VerticalCandidate(id = 0L, cut = cut, intent = sortedIntent))
// result.accepted is structural acceptance, not a complete JSM hypothesis.
```

V also stores `maxPrefixSupport`, the maximum support of any original attribute
below the cut, computed once from the original rows (not overlapping H counts).
For a V-closed candidate with support `s > maxPrefixSupport`, no omitted attribute
can be common to its extent: such an attribute would itself need support at least
`s`. It is therefore structurally accepted without H checks. Equality is NOT
sufficient. With an empty prefix (`cut = 0`), no H checks are needed either.
This bound belongs to the fixed context and must not be reused with different data.

The local reference verifier recomputes exact support and suffix closure using V,
then skips H checks when this support bound applies (`checks` is empty). It still
enforces projected closure, nonempty intent, and minimum support; the bound alone
does not validate an arbitrary input intent or perform JSM property filtering.
Each reusable `HorizontalCandidateVerifier` scans matching rows in its fixed H
context and intersects only its owned prefix attributes. It reports a missing
attribute only when its matching-row count equals V's global support. If its count
is smaller, some supporting object is outside that H context, so no attribute
owned by H can be common to all supporting objects. H contexts overlap: their
counts must never be summed. The standalone H checker expects the exact support
from V and a payload produced by this plan, not an arbitrary row partition.

The replies reconstruct the omitted prefix closure. A nonempty candidate is
accepted only if it meets minimum support, is closed within V, and has no omitted
attributes. Empty extents have the full attribute universe as their mathematical
closure, but fail positive minimum support. Candidate sets, replies, and fixed
payloads are read-only. Checks do not inspect or advance any CbO continuation.

Output ownership is by minimum attribute: H owns minima below the cut, and V owns
accepted suffix-only intents. Rejecting a suffix candidate with extra prefix
attributes does not lose that full concept: its owner is an H shard. Exhaustive
small-context tests compare this union, including multiplicities, with a direct
full-context closure oracle.

V batches merge/filter JSM properties as described below, and the hybrid driver
routes checks through Spark. Outside the support shortcut, verification currently
checks every H shard; per-shard selective routing and early exit remain future work.

The supplied Xu et al. paper (ICFCA 2012, section 3) explains closure combination
for disjoint row partitions. The trailing-attribute cut here is our extension;
the support-count guard above is necessary for this project's overlapping H
contexts rather than a direct use of the paper's partition assumptions.

## Resumable V candidate batches

`VerticalRound.advance(vertical, metadata, options, maxCandidates, state)` runs
the same resumable CbO on V's projected rows. It stops at either `options.roundSteps`
transitions or `maxCandidates` emitted candidates, whichever comes first. The new
`ResumableCbO.advanceUntil` hook checks the stop condition between transitions,
without changing the existing `advance` behavior. Root initialization and closure
scans are still atomic, so the step budget is not a wall-clock deadline.

Each `VerticalBatch` contains `candidates`, `state`, CbO `progress`, and the
per-round `rejectedProperties` count. Each
`SupportedVerticalCandidate` pairs the existing `VerticalCandidate` request with
its exact support from V's extent and nonempty merged `properties`, encoded as a
string using the FIMI header's property factory. `requiresHorizontalCheck` is false when the
prefix support bound proves full-context closure, so these candidates can bypass
H routing and are ready for model output after restoring original attribute IDs.
Candidates are nonempty, meet minimum support, and are closed in the projection;
those still requiring H checks may fail verification. They retain
global sorted IDs, not original IDs. Sets are shared read-only with CbO frames;
batching does not copy extent/intent arrays.

`VerticalState` stores the CbO continuation, minimum support, merge strategy, and next candidate
ID. IDs start at 0, are unique within one V run, and are stable across batch sizes
and replay. IDs are assigned only to candidates that pass property filtering.
Restore requires the same fixed context, properties, cut, ordering, support, and
strategy. Changing strategy on restore is rejected.
Changing only the step or candidate budget is allowed. Java serialization covers
the complete batch. The hybrid driver commits candidates and continuation
together; the local API itself does not persist or deliver a batch. Binary V
batches/states written before property filtering are not compatible with this format.

```scala
val options = MiningOptions(splits = 5, support = 5, roundSteps = 10000)
val plan = VerticalPlan.prepare(data, options, cut)
val verifier = new LocalVerticalVerifier(plan, options.support)
var state = Option.empty[VerticalState]
val records = Vector.newBuilder[String]
do {
  val batch = VerticalRound.advance(plan.vertical, plan.metadata, options,
    maxCandidates = 256, state = state)
  batch.candidates.foreach { item =>
    val accepted = !item.requiresHorizontalCheck || verifier.verify(item.candidate).accepted
    if (accepted) {
      val intent = item.candidate.intent.map(plan.metadata.order).toSeq.sorted.mkString(" ")
      records += intent + " | " + item.properties
    }
  }
  state = Some(batch.state)
} while (!state.get.done)
val verticalModelRecords = records.result() // Headerless, like H's model records.
```

An empty batch may still have unfinished work: use `state.done`, not batch size,
for termination. A finished state returns no candidates and zero steps.
`maxCandidates = 0` provides backpressure with zero mining steps (initializing a
brand-new miner still builds its root). Replaying the same input state returns the
same candidates and IDs; callers must not treat a replay as an additional delivery.
The lower-level H checker can use `item.support` directly with
`check(item.candidate, item.support)` instead of recomputing support from V.

## V property filtering

Before enqueuing a candidate (including candidates eligible for the support fast
path), V merges the properties of its supporting objects using the original
`Algorithm.merge` and `Context.createContext` strategy selection. V's projection
retains every row in original order, so this is the same extent and property order
as in the original full context. Duplicate objects retain separate support and
votes. Supported strategies are `noCounterExamples`, `votingMajority`, and
`boundedVotingMajority:0..100`; this reuses existing semantics, including tau,
empty, ordinal, and composite properties, rather than defining new voting rules.

Only `merged.empty` suppresses a property result, matching CbO: a partially empty
composite can still be emitted. Serialized candidates carry the factory's text
encoding, not live property/factory instances. H checks remain purely structural;
they do not merge overlapping property votes or repeat V's property work.

A property rejection increments `rejectedProperties` but does not consume a
candidate ID or candidate-buffer slot, and NEVER prunes the CbO subtree. A rejected
parent can have valid descendants. The CbO step budget still bounds rounds that
produce few or no surviving candidates; an empty batch alone is not completion.

Tests compare candidate order/support against recursive CbO for all
512 three-row/three-attribute contexts, every cut, three support thresholds, and
three step/candidate budget combinations, plus serialized replay and H/V union.
Mixed-property tests compare complete H+V records (including multiplicities) with
the original CLI across all two-attribute/three-row binary contexts, every binary
label assignment and cut, support thresholds, strategies, and batch limits.
Additional cases cover ordinal/composite encoding, tau/empty values, strict voting
thresholds, support shortcuts, and rejected parents with valid descendants.

## Compare with original CbO

```bash
target/universal/stage/bin/jsm-cli generate -a cbo --support 5 \
  -m mushroom-reference.dat mushroom-training.dat
LC_ALL=C sort mushroom-reference.dat > mushroom-reference.sorted
LC_ALL=C sort mushroom-spark-model.dat > mushroom-spark.sorted
diff -u mushroom-reference.sorted mushroom-spark.sorted
```

Use plain `sort`, not `sort -u`, to detect multiplicity differences. The tests
exhaustively compare all three-row/three-attribute contexts against a closure
oracle across support and split settings, check the duplicate-root regression,
property filtering, serialization, and actual Spark output/overwrite refusal.
Resumable tests compare exact DFS order and closure/canonicality statistics with
recursive CbO across 512 contexts and three budgets. Round tests cover repeated
snapshot/restore, uneven/empty shards, one-step rounds, different budgets, and a
forced failed task attempt followed by successful replay without duplicate output.

## Deployment and sizing

`bash scripts/spark-deploy.sh user@gateway release-name` uploads the built assembly
and launch/collection scripts under an existing `/srv/jsm/releases` directory.
The release name must be new. On the gateway set `JSM_SPARK_JAR` to the uploaded
JAR and `JSM_SPARK_MASTER` to the cluster master, then invoke the uploaded wrapper.
Spark distributes the JAR to executors. Fetch the completed result directory with
rsync, then run the collector locally.

This first version requires driver memory for the full input plus the replicated
shard payloads, and task memory for a whole context, explicit search stack, and a
bounded output batch. A shard executes at most one mining task in each round; more
physical cores cannot subdivide one expensive context. Round boundaries add jobs,
serialization and storage I/O; use larger budgets when no V checks need low latency.
In hybrid mode, V's projected context adds memory, and every pending candidate may
scan each H context. Start with modest candidate batches; measure verification time
alongside mining time before increasing the batch size or selecting a wider V suffix.
Increase split count cautiously: selected rows can be replicated across
all splits. Start with one task CPU and modest executor concurrency; measure task
duration and memory before enabling more cores. Keep input files immutable during
a run. Record the deployed JAR checksum alongside experiment results.

Official references: [Spark 3.5.9 compatibility](https://spark.apache.org/docs/3.5.9/),
[submission and JAR distribution](https://spark.apache.org/docs/3.5.9/submitting-applications.html),
[standalone cluster](https://spark.apache.org/docs/3.5.9/spark-standalone.html).
