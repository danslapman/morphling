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
    val result = Person.schema.toBson(person)
    result shouldBe document(
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
    val result = Person.schema.toBson(person)
    Person.schema.fromBson(result) shouldBe Success(person)
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.toGen)
    check {
      (p: Person) =>
        Person.schema.fromBson(Person.schema.toBson(p)).toOption == Some(p)
    }
  }
}
