package jsm4s.property

case class Properties(val value: Seq[Property]){
  def &(props: Properties): Properties = Properties(value.zip(props.value).map(p => p._1 & p._2))

  def empty = value.find(p => !p.empty).isEmpty
}
