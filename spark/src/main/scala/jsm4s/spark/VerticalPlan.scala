package jsm4s.spark

import jsm4s.FIMI

/** All original objects, in original row order, projected onto [cut, attributes).
  * Duplicate and empty projected rows must be retained for support/properties.
  * Attribute IDs remain in globally sorted coordinates, not rebased to zero.
  */
case class VerticalPayload(cut: Int, rows: Array[Array[Int]], properties: Array[String],
                           maxPrefixSupport: Int) extends Serializable {
  require(maxPrefixSupport >= 0 && maxPrefixSupport <= rows.length,
    "Invalid maximum prefix-attribute support")
  require(cut != 0 || maxPrefixSupport == 0, "An empty prefix must have zero maximum support")

  /** For a V-closed candidate, a prefix attribute in its full closure would need
    * support >= candidate support. Equality cannot rule that attribute out.
    * This bound must come from the same original context as the projection.
    */
  def canSkipHorizontalChecks(support: Int): Boolean = cut == 0 || support > maxPrefixSupport
}

/** Fixed payloads are read-only, like the existing horizontal SplitPlan payloads. */
case class VerticalPlan(metadata: SplitMetadata, horizontal: Vector[SplitPayload],
                        vertical: VerticalPayload) extends Serializable

object VerticalPlan {
  /** H owns minimum attributes in [0, cut); V owns closed nonempty suffix intents.
    * H retains full rows: its descendants may include attributes beyond the cut.
    * cut=0 and cut=attributes are useful all-V/all-H boundary cases.
    */
  def prepare(data: FIMI, options: MiningOptions, cut: Int): VerticalPlan = {
    require(cut >= 0 && cut <= data.attrs, "Vertical cut must be within [0, attributes]")
    val sorted = SplitPlan.sort(data, options)
    val horizontal = SplitPlan.partition(sorted.rows, sorted.properties, options.splits, cut)
    val prefixSupports = Array.fill(cut)(0)
    sorted.rows.foreach(_.foreach { a => if (a < cut) prefixSupports(a) += 1 })
    val maximum = if (prefixSupports.isEmpty) 0 else prefixSupports.max
    val vertical = VerticalPayload(cut, sorted.rows.map(_.filter(_ >= cut)), sorted.properties, maximum)
    VerticalPlan(sorted.metadata, horizontal, vertical)
  }
}
