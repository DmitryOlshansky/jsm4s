package jsm4s

import java.util.concurrent.{Executors, TimeUnit}

import scala.collection.{Seq, mutable}


trait GenericBCbO extends GenericAlgorithm{

  var recDepth = 0

  def method(A:FcaSet, B:FcaSet, y:Int):Unit = {
    val q = mutable.Queue[(FcaSet, FcaSet, Int)]()
    output(A,B)
    for(j <- y until attributes) {
      if(!B.contains(j)){
        val ret = closeConcept(A, j)
        if(ret._1){
          val C = ret._2
          val D = ret._3
          if (B.until(j) == D.until(j)) q.enqueue((C, D, j+1))
          else onCanonicalTestFailure()
        }
      }
    }
    recDepth +=1
    while(!q.isEmpty) processQueue(q.dequeue())
    recDepth -=1
  }

  def run = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection
    method(A, B, 0)
  }
}

abstract class WaveFrontBCbO(rows:Seq[FcaSet], attrs:Int, threads:Int, cutOff:Int, tid:Int)
  extends Algorithm(rows, attrs) with GenericBCbO{
  var counter = 0

  override def output(extent:FcaSet, intent:FcaSet) =
    if(recDepth >= cutOff || tid == 0) super.output(extent, intent)

  def processQueue(value: AnyRef): Unit = {
    val x = value.asInstanceOf[(FcaSet, FcaSet, Int)]
    if(recDepth != cutOff)
      method(x._1, x._2, x._3) // main thread < cutOff or post cutOff
    else{
      // round-robin among threads, each remember their tid
      if (counter == tid)
        method(x._1, x._2, x._3)
      counter += 1
      if(counter == threads) counter = 0
    }
  }

  def fork(tid:Int): WaveFrontBCbO // creates concrete descendant type

  override def run = {
    val A = fullExtent
    val B = rows.fold(fullIntent)((a,b) => a & b) // full intersection

    val pool = Executors.newFixedThreadPool(threads)
    for(t <- 0 until threads)
      pool.submit(new Runnable(){
        val algo = fork(t)
        def run = {
          algo.method(A, B, 0)
        }
      })
    pool.shutdown
    pool.awaitTermination(10, TimeUnit.DAYS)
  }
}