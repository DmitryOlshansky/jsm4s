package jsm4s

import java.io._

import com.typesafe.scalalogging.LazyLogging
import jsm4s.algorithm._
import jsm4s.Utils._
import jsm4s.algorithm.Strategies.MergeStrategy
import jsm4s.property.Properties

object JSM extends LazyLogging {

  def generate(input: InputStream, output: OutputStream, algorithm: String, dataStructure: String, minSupport: Int, threads: Int) = {
    val data = FIMI.load(input)
    val sink = new StreamSink(data.header, output)
    val stats = new SimpleCollector
    val jsm = Algorithm(algorithm, dataStructure, data, minSupport, threads, stats, sink)
    jsm.run()
  }

  def predict(model: File, tau: File, output: File, mergeStrategy: MergeStrategy) = {
    val hypotheses = timeIt("Loading hypotheses")(FIMI.load(new FileInputStream(model)))
    val examples = timeIt("Loading examples")(FIMI.load(new FileInputStream(tau)))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try{
      if (hypotheses.header != examples.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${hypotheses.header}` vs `${examples.header}`")
      out.write(hypotheses.header+"\n")
      val combined = hypotheses.intents.zip(hypotheses.props).map{ x => Hypothesis(x._1, x._2) }
      val predictor = new Predictor(combined, hypotheses.attrs, mergeStrategy)
      val predictions = timeIt("Calculating predictions")(examples.intents.par.map { e => (e, predictor(e)) }).seq
      timeIt("Predictions serialization"){
        for (p <- predictions) out.write(p._1.mkString("", " ", " | ") + p._2.toString + "\n")
      }
    }
    catch {
      case e: Exception =>
        output.delete()
        throw e
    }
    finally out.close()
  }

  def refine(model: File, crossValidation: File, output: File, mergeStrategy: MergeStrategy) = {
    val hypotheses = timeIt("Loading hypotheses")(FIMI.load(new FileInputStream(model)))
    val validation = timeIt("Loading examples")(FIMI.load(new FileInputStream(crossValidation)))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try{
      if (hypotheses.header != validation.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${hypotheses.header}` vs `${validation.header}`")
      out.write(hypotheses.header+"\n")
      val combined = hypotheses.intents.zip(hypotheses.props).map{ x => Hypothesis(x._1, x._2) }
      val validator = new CrossValidator(combined, hypotheses.attrs, mergeStrategy)
      val wronglyPredicted = timeIt("Cross-validating the model") {
        validation.intents.zip(validation.props).par.filter {
          case (intent, props) => !validator(intent, props)
        }.seq
      }
      logger.info(s"Wrongly predicted in cross-validation: ${wronglyPredicted.size}")
      // try to construct "gradient" model using only wrongly predicted cases
      val sink = new ArraySink()
      val stats = new SimpleCollector
      val fimi = FIMI(wronglyPredicted.map(_._1), wronglyPredicted.map(_._2), hypotheses.attrs, hypotheses.header)
      val algo = Algorithm("pfcbo", "dense", fimi, 2, 0, stats, sink)
      algo.run()
      logger.info(s"Gradient hypotheses size: ${sink.hypotheses.size}")
      timeIt("Refined model serialization"){
        for (p <-  sink.hypotheses)
          out.write(p.intent.mkString("", " ", " | ") + p.props.toString + "\n")
      }
    }
    catch {
      case e: Exception =>
        output.delete()
        throw e
    }
    finally out.close()
  }

  def jsm(input: File, tau: File, output: File, algorithm: String, dataStructure: String, minSupport: Int,
          threads: Int, mergeStrategy: Seq[Properties]=>Properties) = {
    val training = timeIt("Loading training examples")(FIMI.load(new FileInputStream(input)))
    val examples = timeIt("Loading tau examples")(FIMI.load(new FileInputStream(tau)))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try {
      if (training.header != examples.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${training.header}` vs `${examples.header}`")
      val sink = new ArraySink()
      val stats = new SimpleCollector
      val algo = Algorithm(algorithm, dataStructure, training, minSupport, threads, stats, sink)
      timeIt("Generating hypotheses")(algo.run())
      val hypotheses = sink.hypotheses
      val predictor = new Predictor(hypotheses, training.attrs, mergeStrategy)
      val predictions = timeIt("Calculating predictions")(examples.intents.par.map { e => (e, predictor(e)) }).seq
      timeIt("Predictions serialization"){
        out.write(training.header+"\n")
        for (p <- predictions) out.write(p._1.mkString("", " ", " | ") + p._2.toString + "\n")
      }
    }
    catch {
      case e: Exception =>
        output.delete()
        throw e
    }
    finally {
      out.close()
    }
  }

  def stats(validation: File, prediction: File) = {
    val valid = FIMI.load(new FileInputStream(validation))
    val predicted = FIMI.load(new FileInputStream(prediction))
    if (valid.header != predicted.header)
      throw new JsmException(s"Metadata of data sets doesn't match `${valid.header}` vs `${predicted.header}`")
    if (valid.props.exists(p => p.tau))
      throw new JsmException(s"Presence of tau examples in verification data sets (swapped the arguments?)")

    val attrs = valid.attrs
    val pairs = valid.props.zip(predicted.props)
    val correct = pairs.count { case (a, b) => a == b }
    val unknown = predicted.props.count { x => x.tau }
    val conflicts = predicted.props.count { x => x.empty }

    logger.info("Correct predictions ratio {}/{}", correct, pairs.size)
    logger.info("Unknown ratio {}/{}", unknown, pairs.size)
    logger.info("Conflicts ratio {}/{}", conflicts, pairs.size)
  }
}

class JsmException(message: String) extends Exception(message)
