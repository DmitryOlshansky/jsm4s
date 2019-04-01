package jsm4s

import java.io._

import com.github.tototoshi.csv.CSVReader
import jsm4s.attribute.{Attribute, EnumAttribute}
import jsm4s.ds.{BitSet, FcaSet}
import jsm4s.property.{BinaryProperty, Composite, Property, PropertyFactory}

import scala.collection.{Seq, SortedMap, SortedSet, mutable}
import scala.io.Source
import scala.util.Random

case class FIMI(intents: Seq[FcaSet],
                props: Seq[Property],
                header: String,
                attrs: Int,
                factory: PropertyFactory)

object FIMI {

  private val regex = """#\s+attributes:\s+(\d+)\s+properties:\s+([\s<=>?(),\w]+)""".r

  private def parseFimiHeader(header: String) = {
    val regex(attrsDescr, propertyDescr) = header
    (attrsDescr.toInt, PropertyFactory(propertyDescr))
  }

  def encode(input: InputStream, output: OutputStream, properties: List[Int]) = {
    val reader = CSVReader.open(new InputStreamReader(input))
    // for each index: value --> property --> count of records with this value
    val valuesDistribution = mutable.Map[Int, mutable.Map[String, mutable.Map[String, Int]]]()
    val values = mutable.Buffer[Seq[String]]()
    for (line <- reader) {
      if (line != List("")) {
        values += line
        val row = line.zipWithIndex
        row.foreach { x =>
          val attribute = x._1
          val perAttribute = valuesDistribution.getOrElseUpdate(x._2, mutable.Map.empty)
          properties.foreach { p =>
            val props = perAttribute.getOrElseUpdate(attribute, mutable.Map.empty)
            props.put(line(p), props.getOrElse(line(p), 0) + 1)
          }
        }
      }
    }

    var lastUsed = 0
    // First map normal attributes that are not properties
    val attributesTranslation = valuesDistribution.map {
      case (index, _) =>
        if(properties.contains(index)) index -> null
        else {
          val ret = Attribute(valuesDistribution(index).toMap.map {
            case (k,v ) => k -> v.toMap
          }, lastUsed)
          lastUsed += ret.size
          index -> ret
        }
    }

    // Now map properties
    val propertiesTranslation = mutable.HashMap[(String, Int), String]()
    var propertiesDescriptor = ""
    for ((k, v) <- valuesDistribution if properties.contains(k)) {
      if (v.size == 2) {
        val keys = v.keys.toSeq.sorted
        val factory = new BinaryProperty.Factory(keys)
        for (item <- keys) {
          propertiesTranslation.put((item, k), factory.decode(factory.encode(item)))
        }
        val descriptor = "B" + keys.mkString("(", ",", ")")
        propertiesDescriptor += descriptor
      }
      else ??? // other properties
    }

    val attributes = lastUsed
    output.write(s"# attributes: $attributes properties: $propertiesDescriptor\n".getBytes("UTF-8"))
    for (line <- values) {
      val attrs = line.zipWithIndex.filter(p => !properties.contains(p._2))
        .flatMap { pair => attributesTranslation(pair._2)(pair._1) }.sorted.mkString(" ")
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
      val factory = new Composite.Factory(propertiesDescr)
      val tau = factory.decode(factory.tau)
      writer.write(header + "\n")
      for (line <- lines) {
        val attrs = line.split(" \\| ")(0)
        writer.write(attrs + " | " + tau + "\n")
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
    val (attrs, factory) = parseFimiHeader(header)
    val intents = mutable.Buffer[FcaSet]()
    val properties = mutable.Buffer[Property]()
    for (line <- lines) {
      val parts = line.split(" \\| ")
      val attrsIterable = parts(0).split(" ").map(_.toInt)
      intents += BitSet(attrsIterable, attrs)
      properties += factory.encode(parts(1).trim)
    }
    FIMI(intents, properties, header, attrs, factory)
  }
}