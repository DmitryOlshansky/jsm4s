package me.olshansky

import java.io.FileInputStream

import com.typesafe.scalalogging.LazyLogging
import jsm4s.JSM.logger
import jsm4s.{FIMI, JSM}
import jsm4s.algorithm._
import jsm4s.ds.FcaSet
import jsm4s.property.Properties
import jsm4s.Utils._

import scala.collection.mutable
import scala.util.Random

class BaggedPredictor(private val predictors: Seq[Predictor]) extends (FcaSet => Properties) {
  def apply(intent: FcaSet): Properties = {
    val votes = mutable.Map[Properties, Int]()
    predictors.map(_(intent)).groupBy(id => id).maxBy(_._2.size)._1
  }
}

object Script extends App with LazyLogging {
  val dataset = "adult"
  val folds = 8
  val algo = "pfcbo"

  val src = FIMI.load(new FileInputStream(s"$dataset-training.dat"))
  val tau = FIMI.load(new FileInputStream(s"$dataset-tau.dat"))
  val verify = FIMI.load(new FileInputStream(s"$dataset-verify.dat"))
  val rng = new Random()

  val predictors = 0.until(folds).par.map { _ =>
    val sink = new ArraySink()
    val stats = new SimpleCollector
    val size = src.props.size
    val sampled = rng.shuffle(0.until(size).toList).slice(0, size / (4*folds))
    val subset = FIMI(sampled.map(src.intents), sampled.map(src.props), src.attrs, src.header)
    timeIt("Generation of a fold model") {
      Algorithm(algo, "dense", subset, 2, 0, stats, sink).run()
    }
    new Predictor(sink.hypotheses, src.attrs, Strategies.votingMajority)
  }.seq
  val bagged = new BaggedPredictor(predictors)

  val predictions = timeIt("Bagged predictions took") {
    tau.intents.par.map(bagged).seq
  }
  val pairs = verify.props.zip(predictions)
  val correct = pairs.count { case (a, b) => a == b }
  val unknown = predictions.count { x => x.tau }
  val conflicts = predictions.count { x => x.empty }

  logger.info("Correct predictions ratio {}/{}", correct, pairs.size)
  logger.info("Unknown ratio {}/{}", unknown, pairs.size)
  logger.info("Conflicts ratio {}/{}", conflicts, pairs.size)
}
