package jsm4s

import java.io._

import com.typesafe.scalalogging.{LazyLogging, Logger}
import org.rogach.scallop.{ScallopConf, Subcommand}

object EncodeCommand extends Subcommand("encode") {
  val properties = opt[List[Int]](short = 'p', descr = "Number of properties")
  val input = trailArg[File](descr = "Input CSV file", required = false)
  val output = trailArg[File](descr = "Output FIMI file", required = false)
}

object SplitCommand extends Subcommand("split") {
  val ratio = trailArg[String]()
  val input = trailArg[File]()
  val first = trailArg[File]()
  val second = trailArg[File]()
}

object TauCommand extends Subcommand("tau") {
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val input = trailArg[File]()
  val output = trailArg[File]()
}

object RecognizeCommand extends Subcommand("recognize") {
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val model = opt[File](short = 'm', descr = "File with model that contains hypotheses")
  val output = opt[File](short = 'o', descr = "Output file with predictions")
  val debug = opt[Boolean](short = 'd', descr = "Debug mode - output hypotheses for each example")
  val tau = trailArg[File](descr = "File with Tau examples to predict")
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

object StatsCommand extends Subcommand("stats") {
  val properties = opt[Int](short = 'p', descr = "Number of properties")
  val validation = trailArg[File]()
  val prediction = trailArg[File]()
}

class Config(arguments: Seq[String]) extends ScallopConf(arguments) {
  addSubcommand(EncodeCommand)
  addSubcommand(SplitCommand)
  addSubcommand(TauCommand)
  addSubcommand(GenerateCommand)
  addSubcommand(RecognizeCommand)
  addSubcommand(JsmCommand)
  addSubcommand(StatsCommand)
  verify()
}

object EntryPoint extends LazyLogging{

  var log = Logger(EntryPoint.getClass)

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
        JSM.encode(input, output, e.properties.getOrElse(List()))
      case Some(GenerateCommand) =>
        val g = GenerateCommand
        val output = g.model.map(f => new FileOutputStream(f).asInstanceOf[OutputStream])
          .getOrElse(System.out)
        val sets = g.input.map(f => JSM.load(new FileInputStream(f)))
          .getOrElse(JSM.load(System.in))
        val jsm = JSM.generate(
          g.algorithm.getOrElse("fcbo"), sets._1, sets._2,
          g.minSupport.getOrElse(2), g.properties.getOrElse(0)
        )
        jsm.out = output
        jsm.run()
        jsm.printStats()
      case Some(SplitCommand) =>
        val s = SplitCommand
        val Pattern = "([0-9]+):([0-9]+)".r
        val Pattern(firstPart, secondPart) = s.ratio.getOrElse(throw new Exception("Ratio is expected in [0-9]+:[0-9]+ form"))
        (s.input.toOption, s.first.toOption, s.second.toOption) match {
          case (Some(input), Some(first), Some(second)) =>
            JSM.split(input, first, second, firstPart.toInt, secondPart.toInt)
          case _ =>
            log.error("Too few arguments to split command")
        }
      case Some(TauCommand) =>
        val t = TauCommand
        (t.input.toOption, t.output.toOption) match {
          case (Some(input), Some(output)) => JSM.tau(input, output, t.properties.getOrElse(0))
          case _ => log.error("Too few arguments to tau command")
        }
      case Some(RecognizeCommand) =>
        val r = RecognizeCommand
        (r.model.toOption, r.tau.toOption, r.output.toOption, r.properties.toOption) match {
          case (Some(model), Some(tau), Some(output), Some(properties)) =>
            JSM.recognize(model, tau, output, properties, r.debug.getOrElse(false))
          case _ =>
            log.error("Too few arguments to recognize command")
        }
      case Some(StatsCommand) =>
        val s = StatsCommand
        (s.validation.toOption, s.prediction.toOption, s.properties.toOption) match {
          case (Some(validation), Some(prediction), Some(properties)) =>
            JSM.stats(validation, prediction, properties)
          case _ =>
            log.error("Too few arguments to stats command")
        }
      case _ =>
        println("No command specified")
        System.exit(1)
    }
    val after = System.nanoTime()
    val delta = (after - before)/1e9
    logger.info(f"Time: ${delta}%.5f sec")
	}
}