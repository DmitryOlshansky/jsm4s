package jsm4s

import java.io._

import com.typesafe.scalalogging.LazyLogging
import jsm4s.Utils._

import jsm4s.algorithm._
import jsm4s.property.{Composite, Property}
import jsm4s.ds.{ArrayInt, BitInt}
import jsm4s.algorithm.Strategies.MergeStrategy

object JSM extends LazyLogging {

  private def intentFactoryFactory(dataStructure: String ) =
    dataStructure match {
      case "sparse" =>
        (attrs: Int) => new ArrayInt(attrs) 
      case "dense" =>
        (attrs: Int) => new BitInt(attrs)
    }

  def generate(input: InputStream, output: OutputStream, algorithm: String, dataStructure: String, strategy: String, threshold: Double, minSupport: Int, threads: Int) = {
    val factory = intentFactoryFactory(dataStructure)
    val data = FIMI.load(input, factory)
    val sink = new StreamSink(data.header, data.factory, output)
    val stats = new SimpleCollector
    if (strategy == "noop") {
      val groups = data.intents.zip(data.props).groupBy(_._2.key)
      groups.mapValues { g =>
        Algorithm(algorithm, dataStructure, strategy, threshold, g.map(_._1), g.map(_._2), data.attrs, minSupport, threads, stats, sink)
      }.foreach(_._2.run(false))
      sink.close()
    } else {
      val jsm = Algorithm(algorithm, dataStructure, strategy, threshold, data.intents, data.props, data.attrs, minSupport, threads, stats, sink)
      jsm.run()
    }
  }

  def predict(model: File, tau: File, output: File, debug: Boolean, dataStructure: String, mergeStrategy: MergeStrategy) = {
    val factory = intentFactoryFactory(dataStructure)
    val hypotheses = timeIt("Loading hypotheses")(FIMI.load(new FileInputStream(model), factory))
    val examples = timeIt("Loading examples")(FIMI.load(new FileInputStream(tau), factory))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try{
      if (hypotheses.header != examples.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${hypotheses.header}` vs `${examples.header}`")
      out.write(hypotheses.header+"\n")
      val combined = hypotheses.intents.zip(hypotheses.props).map{ x => Hypothesis(x._1, x._2) }
      val predictor = new Predictor(combined, hypotheses.attrs, hypotheses.factory, mergeStrategy)
      val predictions = timeIt("Calculating predictions")(examples.intents.par.map { e => (e, predictor(e)) }).seq
      timeIt("Predictions serialization"){
        for (p <- predictions) out.write(p._1.mkString("", " ", " | ") + hypotheses.factory.decode(p._2) + "\n")
      }
    }
    catch {
      case e: Exception =>
        output.delete()
        throw e
    }
    finally out.close()
  }

  def jsm(input: File, tau: File, output: File, algorithm: String, dataStructure: String, strategy: String, threshold: Double, minSupport: Int,
          threads: Int, debug: Boolean, mergeStrategy: Seq[Property]=>Property) = {
    val factory = intentFactoryFactory(dataStructure)
    val training = timeIt("Loading training examples")(FIMI.load(new FileInputStream(input), factory))
    val examples = timeIt("Loading tau examples")(FIMI.load(new FileInputStream(tau), factory))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try {
      if (training.header != examples.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${training.header}` vs `${examples.header}`")
      val sink = new ArraySink()
      val stats = new SimpleCollector
      val algo = Algorithm(algorithm, dataStructure, strategy, threshold, training.intents, training.props, training.attrs, minSupport, threads, stats, sink)
      timeIt("Generating hypotheses")(algo.run())
      val hypotheses = sink.hypotheses
      val factory = new Composite.Factory(training.header)
      val predictor = new Predictor(hypotheses, training.attrs, training.factory, mergeStrategy)
      val predictions = timeIt("Calculating predictions")(examples.intents.par.map { e => (e, predictor(e)) }).seq
      timeIt("Predictions serialization"){
        out.write(training.header+"\n")
        for (p <- predictions) out.write(p._1.mkString("", " ", " | ") + factory.decode(p._2) + "\n")
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

  def stats(validation: File, dataStructure: String, prediction: File) = {
    val factoryFactory = intentFactoryFactory(dataStructure)
    val valid = FIMI.load(new FileInputStream(validation), factoryFactory)
    val predicted = FIMI.load(new FileInputStream(prediction), factoryFactory)
    if (valid.header != predicted.header)
      throw new JsmException(s"Metadata of data sets doesn't match `${valid.header}` vs `${predicted.header}`")
    if (valid.props.exists(p => p.tau))
      throw new JsmException(s"Presence of tau examples in verification data sets (swapped the arguments?)")

    val pairs = valid.props.zip(predicted.props)
    val correct = pairs.count { case (a, b) => a == b }
    val unknown = predicted.props.count { x => x.tau }
    val conflicts = predicted.props.count { x => x.empty }

    logger.info("Correct predictions ratio {}/{} {}%", correct, pairs.size, correct.toDouble*100/pairs.size)
    logger.info("Unknown ratio {}/{} {}%", unknown, pairs.size, unknown.toDouble*100/pairs.size)
    logger.info("Conflicts ratio {}/{} {}%", conflicts, pairs.size, conflicts.toDouble*100/pairs.size)
  }
}

class JsmException(message: String) extends Exception(message)
