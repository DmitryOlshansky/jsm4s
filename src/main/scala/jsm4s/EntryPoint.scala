package jsm4s

import java.io._

import com.typesafe.scalalogging.LazyLogging
import org.rogach.scallop.{ScallopConf, Subcommand}

object EncodeCommand extends Subcommand("encode") {
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val input = trailArg[File](descr = "Input CSV file", required = false)
  val output = trailArg[File](descr = "Output FIMI file", required = false)
}

object SplitCommand extends Subcommand("split") {

}

object TauCommand extends Subcommand("tau") {

}

object RecognizeCommand extends Subcommand("recognize") {

}

object GenerateCommand extends Subcommand("generate") {
  val algorithm = opt[String](default = Some("fcbo"), short = 'a', descr = "One of: cbo, fcbo, dynsort-cbo, wf-cbo, wf-fcbo")
  val minSupport = opt[Int](name = "support", descr = "Minimum number of objects to support hypothesis")
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val model = opt[File](short = 'm', descr = "File to store model in")
  val input = trailArg[File](descr = "Input file with examples to train")
}

object JsmCommand extends Subcommand("jsm") {
  val algorithm = opt[String](default = Some("fcbo"), short = 'a', descr = "One of: cbo, fcbo, dynsort-cbo, wf-cbo, wf-fcbo")
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val input = trailArg[File]()
}

class Config(arguments: Seq[String]) extends ScallopConf(arguments) {
  addSubcommand(EncodeCommand)
  addSubcommand(SplitCommand)
  addSubcommand(TauCommand)
  addSubcommand(GenerateCommand)
  addSubcommand(RecognizeCommand)
  addSubcommand(JsmCommand)
  verify()
}

object EntryPoint extends LazyLogging{
	def main(args: Array[String]) = {
    val config = new Config(args)
    val before = System.nanoTime()
		config.subcommand match {
      case Some(EncodeCommand) =>
        val e = EncodeCommand
        val input = e.input.map(f => new FileInputStream(f).asInstanceOf[InputStream])
          .getOrElse(System.in)
        val output = e.output.map(f => new FileOutputStream(f).asInstanceOf[OutputStream])
          .getOrElse(System.out)
        FIMI.encode(input, output)
      case Some(GenerateCommand) =>
        val g = GenerateCommand

        val output = g.model.map(f => new FileOutputStream(f).asInstanceOf[OutputStream])
          .getOrElse(System.out)
        val sets = g.input.map(f => FIMI.load(new FileInputStream(f)))
          .getOrElse(FIMI.load(System.in))
        val jsm = FIMI.algorithm(
          g.algorithm.getOrElse("fcbo"), sets._1, sets._2,
          g.minSupport.getOrElse(0), g.properties.getOrElse(0)
        )
        jsm.out = output
        jsm.run()
        jsm.printStats()
      case _ =>
        println("No command specified")
        System.exit(1)
    }
    val after = System.nanoTime()
    val delta = (after - before)/1e9
    logger.info(f"Time: ${delta}%.5f sec")
	}
}