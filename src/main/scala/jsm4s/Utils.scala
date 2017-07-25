package jsm4s

import com.typesafe.scalalogging.LazyLogging

object Utils extends LazyLogging {

  def timeIt[T](name: String)(fn: => T) = {
    val start = System.nanoTime()
    val ret = fn
    val end = System.nanoTime()
    val delta = Math.round((end - start)/1e6)
    logger.info(name + " took {} seconds", delta / 1000.0)
    ret
  }
}
