package jsm4s
import java.util.Arrays

class SortedArray(var table:Array[Int], var tsize:Int) extends FcaSet with Serializable{

  override def contains(elem: Int): Boolean = {
    Arrays.binarySearch(table, 0, tsize, elem) >= 0
  }

  override def iterator: Iterator[Int] = new Iterator[Int]{
    var i = 0
    override def hasNext: Boolean = i != table.length

    override def next(): Int = {
      val item = table(i)
      i += 1
      item
    }
  }

  override def &(set: FcaSet): FcaSet = {
    val target = set.asInstanceOf[SortedArray]
    val result = Array.ofDim[Int](math.min(tsize, target.tsize))
    val rsize = UIntSetOps.intersect(table, tsize, target.table, target.tsize, result)
    new SortedArray(result, rsize)
  }

  override def until(j: Int): FcaSet = {
    new SortedArray(Arrays.copyOf(table, j), j)
  }

  override def +=(x: Int): FcaSet = {

    if(table.length == tsize) {
      val newSize = if(table.length == 0) 4 else table.length*2
      table = Arrays.copyOf(table, newSize)
    }
    table(tsize) = x
    tsize += 1
    this
  }

  override def dup: FcaSet = new SortedArray(Arrays.copyOf(table, table.length), tsize)

  def max: Int = if(table.length > 0) table(table.length-1) else 0

  def ==(that:FcaSet):Boolean = {
    val target = that.asInstanceOf[SortedArray]
    for(i <- 0 until math.min(tsize, target.tsize)){
      if(table(i) != target.table(i)) return false
    }
    true
  }
}

object SortedArray{
  val empty = new SortedArray(Array[Int](), 0)
}