package morphling.reactivemongo

import morphling.reactivemongo.FromBson._
import morphling.reactivemongo.Implicits._
import morphling.reactivemongo.ToBson._
import morphling.samples._
import morphling.samples.Person
import morphling.scalacheck.Implicits._
import morphling.scalacheck.ToGen._
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, Matchers, TryValues}
import org.scalatestplus.scalacheck.Checkers

import reactivemongo.bson._

import scala.util.Success

class ReactivemongoSpec extends FunSuite with Matchers with TryValues with Checkers {
  test("A value should serialise to BSON") {
    val result = Person.schema.writer.write(person)
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
    val result = Person.schema.writer.write(person)
    Person.schema.reader.readTry(result) shouldBe Success(person)
  }

  test("A default value should be applied during deserialization") {
    val result = Person.schema.writer.write(person).asInstanceOf[BSONDocument]
    Person.schema.reader.readTry(result -- "updateCounter") shouldBe Success(person.copy(updateCounter = 0))
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.gen)
    check {
      (p: Person) => Person.schema.reader.readOpt(Person.schema.writer.write(p)) == Some(p)
    }
  }

  test("A value should serialise to BSON flat") {
    val result = Person.flatSchema.writer.write(person)
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
    val result = Person.flatSchema.writer.write(person)
    Person.flatSchema.reader.readTry(result) shouldBe Success(person)
  }

  test("Flat serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.flatSchema.gen)
    check {
      (p: Person) => Person.flatSchema.reader.readOpt(Person.flatSchema.writer.write(p)) == Some(p)
    }
  }
}
