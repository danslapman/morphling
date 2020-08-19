package morphling.reactivemongo

import scala.util.Success

import morphling.reactivemongo.FromBson._
import morphling.reactivemongo.Implicits._
import morphling.reactivemongo.ToBson._
import morphling.samples.{Person, _}
import morphling.scalacheck.Implicits._
import morphling.scalacheck.ToGen._
import org.scalacheck.Arbitrary
import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import reactivemongo.api.bson._

class ReactivemongoSpec extends AnyFunSuite with Matchers with TryValues with Checkers {
  test("A value should serialise to BSON") {
    val result = Person.schema.writer.writeTry(person).success.value
    result shouldBe document(
      "updateCounter" -> 42,
      "roles" -> array(
        document(
          "administrator" -> document(
            "subordinateCount" -> 0,
            "department" -> "windmill-tilting"
          )
        )
      ),
      "birthDate" -> 20147028000L,
      "name" -> "Kris Nuttycombe"
    )
  }

  test("A value should be deserialised from BSON"){
    val result = Person.schema.writer.writeTry(person).success.value
    Person.schema.reader.readTry(result) shouldBe Success(person.copy(stamp = 101))
  }

  test("A default value should be applied during deserialization") {
    val result = Person.schema.writer.writeTry(person).success.value.asInstanceOf[BSONDocument]
    Person.schema.reader.readTry(result -- "updateCounter") shouldBe Success(person.copy(updateCounter = 0, stamp = 101))
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.gen)
    check {
      (p: Person) => Person.schema.reader.readOpt(Person.schema.writer.writeTry(p).get) == Some(p)
    }
  }

  test("A value should serialise to BSON flat") {
    val result = Person.flatSchema.writer.writeTry(person).success.value
    result shouldBe document(
      "updateCounter" -> 42,
      "roles" -> array(
        document(
          "type" -> "administrator",
          "subordinateCount" -> 0,
          "department" -> "windmill-tilting"
        )
      ),
      "birthDate" -> 20147028000L,
      "name" -> "Kris Nuttycombe"
    )
  }

  test("A value should be deserialised from BSON flat"){
    val result = Person.flatSchema.writer.writeTry(person).success.value
    Person.flatSchema.reader.readTry(result) shouldBe Success(person.copy(stamp = 101))
  }

  test("Flat serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.flatSchema.gen)
    check {
      (p: Person) => Person.flatSchema.reader.readOpt(Person.flatSchema.writer.writeTry(p).get) == Some(p)
    }
  }
}
