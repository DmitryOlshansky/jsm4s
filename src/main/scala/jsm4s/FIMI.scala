package jsm4s

import java.io._

import com.github.tototoshi.csv.CSVReader
import jsm4s.attribute.{Attribute, EnumAttribute}
import jsm4s.ds.{BitSet, FcaSet, IntentFactory}
import jsm4s.property.{BinaryProperty, OrdinalProperty, Composite, Property, PropertyFactory}

import scala.collection.{Seq, SortedMap, SortedSet, mutable}
import scala.io.Source
import scala.util.Random

import org.eclipse.collections.impl.list.mutable.primitive.IntArrayList

case class FIMI(intents: Seq[FcaSet],
                props: Seq[Property],
                header: String,
                attrs: Int,
                factory: PropertyFactory)

object FIMI {

  private val regex = """#\s+attributes:\s+(\d+)\s+properties:\s+([\s<=>?(),\w]+)""".r

  def parseFimiHeader(header: String) = {
    val regex(attrsDescr, propertyDescr) = header
    (attrsDescr.toInt, PropertyFactory(propertyDescr))
  }

  def encode(input: InputStream, output: OutputStream, properties: List[Int]) = {
    val reader = CSVReader.open(new InputStreamReader(input))
    // for each index map of value --> count of records with this value
    val valuesDistribution = mutable.SortedMap[Int, mutable.SortedMap[String, Int]]()
    // for each index map of value --> set of possible properties for this value
    val propertiesDistribution = mutable.SortedMap[Int, mutable.SortedMap[String, Set[String]]]()
    val values = mutable.Buffer[Seq[String]]()
    for (line <- reader) {
      if (line != List("")) {
        values += line
        val row = line.zipWithIndex
        row.foreach { x =>
          valuesDistribution.get(x._2) match {
            case Some(map) => map.get(x._1) match {
              case Some(count) => map.put(x._1, count + 1)
              case None => map.put(x._1, 1)
            }
            case None => valuesDistribution.put(x._2, mutable.SortedMap[String, Int](x._1 -> 1))
          }
          // makes sense only for attributes
          if (!properties.contains(x._2)) {
            val propString = row.filter(pair => properties.contains(pair._2)).map(_._1).mkString
            propertiesDistribution.get(x._2) match {
              case Some(map) => map.get(x._1) match {
                case Some(set) => map.put(x._1, set + propString)
                case None => map.put(x._1, Set(propString))
              }
              case None => propertiesDistribution.put(x._2, mutable.SortedMap[String, Set[String]](x._1 -> Set(propString)))
            }
          }
        }
      }
    }

    var lastUsed = 0
    // First map normal attributes that are not properties
    val attributesTranslation = valuesDistribution.map {
      case (index, values) =>
      if(properties.contains(index)) null
      else {
        val ret = Attribute.factory(values, propertiesDistribution(index), lastUsed)
        lastUsed += ret.size
        ret
      }
    }.toSeq

    // Now map properties as pairs of attributes
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
      else { // oridinal property, works for integers and enums
        val keys = v.keys.toSeq.sorted
        val factory = new OrdinalProperty.Factory(keys)
        for (item <- keys) {
          propertiesTranslation.put((item, k), factory.decode(factory.encode(item)))
        }
        propertiesDescriptor += "O"
      }
    }

    val attributes = lastUsed
    output.write(s"# attributes: $attributes properties: $propertiesDescriptor\n".getBytes("UTF-8"))
    for (line <- values) {
      val attrs = line.zipWithIndex.filter(p => !properties.contains(p._2))
        .flatMap { pair => attributesTranslation(pair._2)(pair._1) }.sorted.mkString(" ")
      val props = line.zipWithIndex.filter(p => properties.contains(p._2))
        .map(propertiesTranslation).mkString("","", "\n")
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

  def load(in: InputStream, factoryFactory: (Int)=>IntentFactory): FIMI  = {
    val lines = Source.fromInputStream(in).getLines()
    val header = lines.next()
    val (attrs, factory) = parseFimiHeader(header)
    val intFactory = factoryFactory(attrs)
    val intents = mutable.Buffer[FcaSet]()
    val properties = mutable.Buffer[Property]()
    val reader = new FIMILineReader()
    for (line <- lines) {
      reader.read(line)
      intents += intFactory.values(reader.attributes)
      properties += factory.encode(reader.properties)
      reader.clear()
    }
    FIMI(intents, properties, header, attrs, factory)
  }
}

class FIMILineReader {
  val table = Array.ofDim[Int](128)
  val attributes = new IntArrayList()
  var properties: String = null
  
  for (i <- 0 until 128) {
    if (i >= '0'.toInt && i <= '9'.toInt) {
      table(i) = i - '0'.toInt
    }
    else if (i == '|') {
      table(i) = -2
    }
    else {
      table(i) = -1
    }
  }

  def read(line: String) {
    var idx = 0
    var value = 0
    var inattr = false
    while (idx < line.length) {
      val ch = table(line(idx))
      if (ch >= 0) {
        value = value * 10 + ch
        inattr = true
      } else if (ch == -1) {
        if (inattr) {
          attributes.add(value)
        }
        value = 0
        inattr = false
      } else if (ch == -2) {
        properties = line.substring(idx+1).trim()
        idx = line.length
      }
      idx += 1
    }
  }

  def clear() {
    properties = null
    attributes.clear()
  }
}