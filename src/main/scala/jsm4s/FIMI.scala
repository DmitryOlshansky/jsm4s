package jsm4s

import java.io._

import com.github.tototoshi.csv.CSVReader
import jsm4s.ds.{BitSet, FcaSet}
import jsm4s.property.{BinaryProperty, Properties, Property}

import scala.collection.{Seq, SortedSet, mutable}
import scala.io.Source
import scala.util.Random

case class FIMI(intents: Seq[FcaSet], props: Seq[Properties], attrs: Int, header:String)

object FIMI {

  val regex = """#\s+attributes:\s+(\d+)\s+properties:\s+(\w+)""".r

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
      intents += BitSet(attrsIterable, attrs)
      if (parts.size == 1)
        properties += new Properties(Seq())
      else
        properties += factory(parts(1))
    }
    FIMI(intents, properties, attrs, header)
  }
}