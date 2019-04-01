package jsm4s.property

import org.scalatest._

class TestProperty extends WordSpec with Matchers {
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

    "factory throws on wrong input" in {
      intercept[PropertyException] {
        BinaryProperty.loader("45")
      }
      intercept[PropertyException] {
        BinaryProperty.loader("-1")
      }
    }

    "factory creates property on correct input" in {
      BinaryProperty.loader("1") shouldEqual  BinaryProperty.Positive
    }
  }

  "Level properties" should {
    "intersection of the same sign picks maximum" in {
      val pos1 = new LevelProperty(3, 1, 0)
      val pos2 = new LevelProperty(3, 3, 0)
      (pos1 & pos2).positive shouldEqual 3
      (pos1 & pos2).negative shouldEqual 0

      val neg1 = new LevelProperty(6, 2, 2)
      val neg2 = new LevelProperty(6, 1, 3)
      (neg1 & neg2).positive shouldEqual 2
      (neg1 & neg2).negative shouldEqual 3
    }

    "intersection of opposite sign eliminates overlap" in {
      val pos1 = new LevelProperty(3, 3, 0)
      val neg1 = new LevelProperty(3, 0, 1)
      (neg1 & pos1).positive shouldEqual 2
      (neg1 & pos1).negative shouldEqual 0

      val pos2 = new LevelProperty(3, 2, 0)
      val neg2 = new LevelProperty(3, 0, 2)
      (neg2 & pos2).positive shouldEqual 1
      (neg2 & pos2).negative shouldEqual 1

      val pos3 = new LevelProperty(3, 2, 1)
      val neg3 = new LevelProperty(3, 1, 2)
      (neg3 & pos3).positive shouldEqual 1
      (neg3 & pos3).negative shouldEqual 1
    }
  }
}
