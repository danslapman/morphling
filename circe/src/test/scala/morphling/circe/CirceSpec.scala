package morphling.circe

import io.circe.literal._
import io.circe.syntax._
import morphling.circe.FromJson._
import morphling.circe.Implicits._
import morphling.circe.ToJson._
import morphling.samples._
import morphling.samples.Person
import morphling.scalacheck.Implicits._
import morphling.scalacheck.ToGen._
import org.scalacheck.Arbitrary
import org.scalatest.{EitherValues, FunSuite, Matchers}
import org.scalatestplus.scalacheck.Checkers

class CirceSpec extends FunSuite with Matchers with EitherValues with Checkers {
  implicit val encoder = Person.schema.encoder
  implicit val decoder = Person.schema.decoder

  test("A value should serialise to JSON") {
    person.asJson shouldBe json"""{"roles":[{"administrator":{"subordinateCount":0,"department":"windmill-tilting"}}],"birthDate":20147028000,"name":"Kris Nuttycombe"}"""
  }

  test("A value should be deserialised from JSON"){
    decoder.decodeJson(person.asJson).right.value shouldBe person
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.toGen)
    check {
      (p: Person) => decoder.decodeJson(p.asJson).toOption == Some(p)
    }
  }
}
