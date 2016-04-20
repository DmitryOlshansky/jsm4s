package jsm4s

import java.io.{FileInputStream, FileOutputStream, InputStream}
import java.util.Scanner

import scala.collection._
import com.typesafe.scalalogging.LazyLogging

case class Config(verbose:Int=1, sort:Boolean=true, algorithm:String="cbo",
	minSupport:Int=0, input:String="", output:String="")

object Miner extends LazyLogging{
	def main(args: Array[String]) = {
		val parser = new scopt.OptionParser[Config]("jsm4s cmd-line tool") {
			head("jsm4s", "v0.2.0") // TODO: get version from sbt-git plugin
			opt[String]('a', "algo") required() valueName("<algorithm>") action {
				(x,c) => c.copy(algorithm=x)
			} text ("'algorithm' is one of cbo, fcbo, tp-cbo, tp-fcbo.")
			opt[String]('i', "input") valueName("<input>") action {
				(x,c) => c.copy(input=x)
			} text ("'input' is input FIMI file.")
			opt[String]('o', "output") valueName("<output>") action {
				(x,c) => c.copy(output=x)
			} text ("'input' is output FIMI file with concepts.")
			opt[Int]('m', "min-supp") valueName("<min-support>") action {
				(x,c) => c.copy(minSupport=x)
			}
		}
		parser.parse(args, Config()) match {
			case Some(Config(verbose, sort, algo, minSupp, in, out)) =>
				val before = System.nanoTime()
				val output = if(out != "") new FileOutputStream(out) else System.out
				val sets = if (in != "") FIMI.load(new FileInputStream(in)) else FIMI.load(System.in)

				val jsm = FIMI.algorithm(algo, sets)
				jsm.minSupport = minSupp
				jsm.sortAttributes = sort
				jsm.out = output
				jsm.run()
				val after = System.nanoTime()
				val delta = (after - before)/1e9
				logger.info(f"Time: ${delta}%.5f sec")
			case None =>

		}
	}
}