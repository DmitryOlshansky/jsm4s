package jsm4s.property

import jsm4s.property
import jsm4s.property.BinaryProperty.{Negative, Positive}
import org.scalatest._

class TestProperty extends WordSpec with Matchers {
  class Fixture {
    val factory = new BinaryProperty.Factory(Seq("a", "b"))
  }
  "Binary properties" should {
    "conflict if of opposite signs" in {
      val pos = BinaryProperty.Positive
      val neg = BinaryProperty.Negative
      pos.positive shouldBe true
      neg.negative shouldBe true
      (pos & neg).empty shouldBe true
    }

    "tau intersected with other properties gives the other property" in {
      val pos = BinaryProperty.Positive
      val neg = BinaryProperty.Negative
      val tau = BinaryProperty.Tau
      (tau & pos) shouldEqual pos
      (tau & neg) shouldEqual neg
    }

    "self intersection is idempotent" in {
      val pos = BinaryProperty.Positive
      val neg = BinaryProperty.Negative
      val tau = BinaryProperty.Tau
      (tau & tau) shouldEqual tau
      (neg & neg) shouldEqual neg
      (pos & pos) shouldEqual pos
    }

    "factory throws on encoding incorrect input" in new Fixture {
      intercept[PropertyException] {
        factory.encode("c")
      }
    }

    "factory encodes valid input" in new Fixture {
      factory.encode("a") shouldBe Positive
      factory.encode("b") shouldBe Negative
    }

    "factory decodes properties" in new Fixture {
      factory.decode(Positive) shouldBe "a"
      factory.decode(Negative) shouldBe "b"
    }
  }
}
