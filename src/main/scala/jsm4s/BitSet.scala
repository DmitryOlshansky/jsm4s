package jsm4s
import java.util.Arrays
import java.lang.Long.numberOfLeadingZeros

class BitSet(var table: Array[Long], var length:Int) extends FcaSet with Serializable{

  def this(seq: Iterable[Int]){
    this(Array[Long](), 0)
    for(s <- seq.iterator) this += s
  }

  override def contains(x: Int): Boolean = {
    if(x/64 < table.length) (table(x/64) & (1L<<(x%64))) != 0
    else false
  }

  override def until(j: Int): FcaSet = {
    val rem = if(j % 64 > 0) 1 else 0
    val mask = 1L<<(j%64)
    val newTable = Arrays.copyOf(table, (j/64)+rem)
    if(rem > 0 ) // mask away upper bits in the last word
      newTable(newTable.length-1) &= (mask-1)
    new BitSet(newTable, j)
  }

  override def &(set: FcaSet): FcaSet = {
    val bitset = set.asInstanceOf[BitSet]
    val newLength = math.min(table.size, bitset.table.size)
    val result = Array.ofDim[Long](newLength)
    var j = 0
    for(i <- 0 until newLength) {
      result(i) = table(i) & bitset.table(i)
      val k = 64 - numberOfLeadingZeros(result(i))
      if (k != 0){
        j = i*64 + k.toInt
      }
    }
    new BitSet(result, j)
  }

  override def +=(x: Int): FcaSet = {
    if(x/64 >= table.size) {
      val newSize = if(table.isEmpty) 4 else table.length*2
      table = Arrays.copyOf(table, newSize)
    }
    length = math.max(x, length)
    table(x/64) |= 1L<<(x%64)
    this
  }

  override def dup: FcaSet = new BitSet(Arrays.copyOf(table, length), length)

  override def max: Int = length

  override def iterator: Iterator[Int] = {
    val len = length
    new Iterator[Int] {
      var i = 0

      override def hasNext: Boolean = {
        while (i < len && (table(i / 64) & (1L << (i % 64))) == 0) {
          i += 1
        }
        i != len
      }

      override def next(): Int = {
        val r = i
        i += 1
        r
      }
    }
  }

  override def ==(set: FcaSet): Boolean = {
    val bitset = set.asInstanceOf[BitSet]
    if(bitset.length != length) return false
    for(i <- 0 until table.size)
      if(table(i) != bitset.table(i)) return false
    true
  }
}

object BitSet {
  def empty = new BitSet(Array[Long](), 0)
  def full(j:Int) = {
    val words = j/64
    val rem = if(j % 64 > 0) 1 else 0
    val mask = 1L<<(j%64)
    val arr = Array.ofDim[Long](words + rem)
    Arrays.fill(arr, -1)
    if(rem > 0) // mask away upper bits in the last word
      arr(arr.length-1) &= (mask-1)
    new BitSet(arr, j)
  }
}