package jsm4s

import java.io.{File, FileInputStream, FileOutputStream, OutputStream}

import com.typesafe.scalalogging.LazyLogging
import org.rogach.scallop.{ScallopConf, Subcommand}

object EncodeCommand extends Subcommand("encode") {

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
  val algorithm = GenerateCommand.algorithm
  val properties = GenerateCommand.properties
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

		config.subcommand match {
      case Some(GenerateCommand) =>
        val g = GenerateCommand
        val before = System.nanoTime()

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
        val after = System.nanoTime()
        val delta = (after - before)/1e9
        jsm.printStats()
        logger.info(f"Time: ${delta}%.5f sec")
      case _ => println("No command specified")
    }
	}
}