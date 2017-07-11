package jsm4s
import java.util.Arrays

class BitSet(val table: Array[Long], val length:Int) extends FcaSet with Serializable{

  def this(seq: Iterable[Int], length: Int){
    this(Array.ofDim[Long]((length+63)/64), length)
    for(s <- seq.iterator) this += s
  }

  override def contains(x: Int): Boolean = {
    if(x < length) (table(x/64) & (1L<<(x%64))) != 0
    else false
  }

  override def until(j: Int): FcaSet = {
    val rem = if(j % 64 > 0) 1 else 0
    val mask = 1L<<(j%64)
    val newTable = Arrays.copyOf(table, table.length)
    for (i <- j/64+rem until table.length)
      newTable(i) = 0
    if(rem > 0) // mask away upper bits in the last word
      newTable(j/64) &= (mask-1)
    new BitSet(newTable, length)
  }

  override def &(set: FcaSet): FcaSet = {
    val bitset = set.asInstanceOf[BitSet]
    val result = Array.ofDim[Long](table.length)
    var i = 0
    while(i < table.size) {
      result(i) = table(i) & bitset.table(i)
      i += 1
    }
    new BitSet(result, length)
  }

  override def +=(x: Int): FcaSet = {
    table(x/64) |= 1L<<(x%64)
    this
  }

  override def dup: FcaSet = new BitSet(Arrays.copyOf(table, table.length), length)

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
    var i = 0
    while(i < table.size) {
      if (table(i) != bitset.table(i)) return false
      i += 1
    }
    true
  }

  override def equalWithMask(that: FcaSet, mask: FcaSet): Boolean = {
    val bitset = that.asInstanceOf[BitSet]
    val bitmask = mask.asInstanceOf[BitSet]
    var i = 0
    while(i < table.size) {
      if ((table(i) & bitmask.table(i)) != (bitset.table(i) & bitmask.table(i))) return false
      i += 1
    }
    true
  }

  override def subsetOf(that: FcaSet, j: Int): Boolean = {
    val bitset = that.asInstanceOf[BitSet]
    val rem = j % 64
    for(i <- 0 until j/64) {
      if((table(i) & bitset.table(i)) != table(i)) return false
    }
    if(rem > 0){
      val mask =  (1L<<rem)-1
      val r = table(j/64) & mask & bitset.table(j/64)
      if(r != (table(j/64) & mask)) return false
    }
    true
  }

  override def size = length
}

object BitSet{
  def empty(size:Int) = new BitSet(Array.ofDim[Long]((size+63)/64), size)
  def full(size:Int) = {
    val words = size/64
    val rem = if(size % 64 > 0) 1 else 0
    val mask = 1L<<(size%64)
    val arr = Array.ofDim[Long](words + rem)
    Arrays.fill(arr, -1L)
    if(rem > 0) // mask away upper bits in the last word
      arr(arr.length-1) &= (mask-1)
    new BitSet(arr, size)
  }
}