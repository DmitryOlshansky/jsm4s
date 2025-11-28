package jsm4s

import org.scalatest._

class TestFIMILineReader extends FlatSpec with Matchers {

    "TestFIMILineReader" should "read FIMI line" in {
        val reader = new FIMILineReader()
        val line = "0 1 2 6 7 8 | 5"
        reader.read(line)
        assert(reader.attributes.size == 6)
        assert(reader.attributes.toArray.toSeq == Seq(0, 1, 2, 6, 7, 8))
        assert(reader.properties == "5")
    }
}