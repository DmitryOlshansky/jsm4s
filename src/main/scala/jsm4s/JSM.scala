package jsm4s

import java.io._
import java.util.Scanner

import com.github.tototoshi.csv.CSVReader
import jsm4s.algorithm._
import jsm4s.ds._
import jsm4s.Utils._
import jsm4s.property.{BinaryProperty, Properties, Property}

import scala.io.Source
import scala.collection.{SortedSet, mutable}
import scala.util.Random


object Strategies {

  def votingMajority(seq: Seq[Properties]): Properties = {
    if (seq.isEmpty) Properties(Seq())
    else {
      val len = seq.head.size
      val votes = Array.fill(len)(mutable.Map[Property, Int]())
      seq.foreach {
        case Properties(props) =>
          for (i <- 0 until len) {
            votes(i).put(props(i), 1 + votes(i).getOrElse(props(i), 0))
          }
      }
      new Properties(votes.map { x => x.maxBy(pair => pair._2)._1 })
    }
  }

}

object JSM {

  private val regex = """#\s+attributes:\s+(\d+)\s+properties:\s+(\w+)""".r

  def encode(input: InputStream, output: OutputStream, properties: List[Int]) = {
    val reader = CSVReader.open(new InputStreamReader(input))
    val uniqueValues = mutable.SortedMap[Int, SortedSet[String]]()
    val values = mutable.Buffer[Seq[String]]()
    for (line <- reader) {
      values += line
      line.zipWithIndex.foreach(x =>
        uniqueValues.get(x._2) match {
          case Some(set) => uniqueValues.put(x._2, set + x._1)
          case None => uniqueValues.put(x._2, SortedSet[String](x._1))
        }
      )
    }

    // Translate value from given position to a sequence of binary attributes
    val attributesTranslation = mutable.HashMap[(String, Int), Option[Int]]()

    var lastUsed = 0
    // First map normal attributes that are not properties
    for ((k, v) <- uniqueValues if !properties.contains(k)) {
      val len = v.size
      for ((item, i) <- v.zipWithIndex) {
        attributesTranslation.put((item, k), Some(lastUsed + i))
      }
      lastUsed += len
    }
    // Now map properties as pairs of attributes
    val propertiesTranslation = mutable.HashMap[(String, Int), Property]()
    var propertiesDescriptor = ""
    for ((k, v) <- uniqueValues if properties.contains(k)) {
      if (v.size == 2) {
        val factory = BinaryProperty.factory(v)
        for ((item, i) <- v.zipWithIndex) {
          propertiesTranslation.put((item, k), factory(item))
        }
        propertiesDescriptor += "B"
      }
      else ??? // other properties
    }

    val attributes = lastUsed
    output.write(s"# attributes: $attributes properties: $propertiesDescriptor\n".getBytes("UTF-8"))
    for (line <- values) {
      val attrs = line.zipWithIndex.filter(p => !properties.contains(p._2))
        .map(attributesTranslation).flatten.sorted.mkString(" ")
      val props = line.zipWithIndex.filter(p => properties.contains(p._2))
        .map(propertiesTranslation).mkString(""," ", "\n")
      output.write((attrs + " | " +  props).getBytes)
    }
    output.close()
  }

  def split(input: File, first: File, second: File, firstPart: Int, secondPart: Int) = {
    val full = firstPart + secondPart
    val rnd = new Random()
    val firstWriter = new OutputStreamWriter(new FileOutputStream(first))
    val secondWriter = new OutputStreamWriter(new FileOutputStream(second))
    val lines = Source.fromFile(input).getLines()
    val header = lines.next()
    firstWriter.write(header+"\n")
    secondWriter.write(header+"\n")
    for (line <- lines) {
      if (rnd.nextInt(full) < firstPart) firstWriter.write(line + "\n")
      else secondWriter.write(line + "\n")
    }
    firstWriter.close()
    secondWriter.close()
  }

  def tau(input: File, output: File) = {
    val lines = Source.fromFile(input).getLines()
    val writer = new OutputStreamWriter(new FileOutputStream(output))
    try{
      val header = lines.next()
      val regex(attrs, propertiesDescr) = header
      val tau = Properties.tau(propertiesDescr)
      writer.write(header + "\n")
      for (line <- lines) {
        val attrs = line.split(" \\| ")(0)
        writer.write(attrs + " | " + tau.toString + "\n")
      }
    }
    catch {
      case e: Exception =>
        output.delete()
        throw e
    }
    finally writer.close()
  }

  def generate(input: InputStream, output: OutputStream, algorithm: String, minSupport: Int) = {
    val data = load(input)
    val sink = new StreamSink(data.header, output)
    val stats = new SimpleCollector
    val jsm = Algorithm(algorithm, data, minSupport, stats, sink)
    jsm.run()
  }

  def recognize(model: File, tau: File, output: File, debug: Boolean, mergeStrategy: (Seq[Properties]=>Properties)) = {
    val hypotheses = timeIt("Loading hypotheses")(load(new FileInputStream(model)))
    val examples = timeIt("Loading examples")(load(new FileInputStream(tau)))
    val out = new OutputStreamWriter(new FileOutputStream(output))
    try{
      if (hypotheses.header != examples.header)
        throw new JsmException(s"Metadata of data sets doesn't match `${hypotheses.header}` vs `${examples.header}`")
      out.write(hypotheses.header+"\n")
      val combined = hypotheses.intents.zip(hypotheses.props).map{ x => Hypothesis(x._1, x._2) }
      val recognizer = new Recognizer(combined, hypotheses.attrs, mergeStrategy)
      val predictions = timeIt("Calculating predictions")(examples.intents.par.map { e => (e,recognizer(e)) }).seq
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

  def stats(validation: File, prediction: File) = {
    val valid = load(new FileInputStream(validation))
    val predicted = load(new FileInputStream(prediction))
    if (valid.header != predicted.header)
      throw new JsmException(s"Metadata of data sets doesn't match `${valid.header}` vs `${predicted.header}`")
    if (valid.props.exists(p => p.tau))
      throw new JsmException(s"Presence of tau examples in verification data sets (swapped the arguments?)")

    val attrs = valid.attrs
    val pairs = valid.props.zip(predicted.props)
    val correct = pairs.count { case (a, b) => a == b }
    var unknown = predicted.props.count { x => x.tau }
    var confilcts = predicted.props.count { x => x.empty }

    println(s"Correct predictions ratio $correct/${pairs.size}")
    println(s"Unknown ratio $unknown/${pairs.size}")
    println(s"Conflicts ratio $confilcts/${pairs.size}")
  }

  def load(in: InputStream): FIMI  = {
    val lines = Source.fromInputStream(in).getLines()
    val header = lines.next()
    val regex(attrsDescr, propertyDescr) = header
    val factory = Properties.loader(propertyDescr)
    val attrs = attrsDescr.toInt
    val intents = mutable.Buffer[FcaSet]()
    val properties = mutable.Buffer[Properties]()

    for (line <- lines) {
      val parts = line.split(" \\| ")
      val attrsIterable = parts(0).split(" ").map(_.toInt)
      intents += new BitSet(attrsIterable, attrs)
      if (parts.size == 1)
        properties += new Properties(Seq())
      else
        properties += factory(parts(1))
    }
    FIMI(intents, properties, attrs, header)
  }
}

class JsmException(message: String) extends Exception(message)
