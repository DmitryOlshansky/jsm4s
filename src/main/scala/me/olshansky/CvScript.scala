package me.olshansky

import java.io.FileInputStream

import com.typesafe.scalalogging.LazyLogging
import jsm4s.FIMI
import jsm4s.Utils._
import jsm4s.algorithm._
import jsm4s.ds.FcaSet
import jsm4s.property.Property
import scala.util.Random

object CvScript extends App with LazyLogging {
  val dataset = "adult"
  val folds = 8
  val algo = "pfcbo"

  val model = FIMI.load(new FileInputStream(s"$dataset-model.dat"))
  val cv = FIMI.load(new FileInputStream(s"$dataset-cv.dat"))
  val verify = FIMI.load(new FileInputStream(s"$dataset-verify.dat"))
  val rng = new Random()

  def validate(hypothesis: Seq[Hypothesis], dataset: FIMI) = {

    val validator = new CrossValidator(hypothesis, model.attrs, model.factory, Strategies.votingMajority)

    val predictions = timeIt("Cross-validation took") {
      dataset.intents.zip(dataset.props).par.map {
        case (i, p) => validator(i,p)
      }
    }.seq
    val correct = predictions.count(_ == true)
    val incorrect = predictions.count(_ == false)
    val filtered = validator.hypotheses.filter(_.wrong.get != 0)
    logger.info(s"Matched ${filtered.size} hypotheses")
    logger.info("Correct ratio {}/{} ({}%)", correct, predictions.size, correct.toDouble*100/predictions.size)
    logger.info("Incorrect ratio {}/{} ({}%)", incorrect, predictions.size, incorrect.toDouble*100/predictions.size)
    validator
  }
  val validator = validate(model.intents.zip(model.props).map(p => Hypothesis(p._1, p._2)), cv)
  val good = validator.hypotheses.filter(_.wrong.get == 0)
  logger.info(s"Number of absolutely good hypotheses ${good.size}")
  validate(good.map(h => Hypothesis(h.intent, h.props)), cv)
  validate(good.map(h => Hypothesis(h.intent, h.props)), verify)
}
