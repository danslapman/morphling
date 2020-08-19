package morphling.circe.annotated

import cats.scalatest.{EitherValues, ValidatedValues}
import io.circe.Json
import io.circe.syntax._
import morphling.circe.FromJson._
import morphling.circe.ToJson._
import morphling.circe.annotated.Implicits._
import morphling.samples.{person, Person}
import morphling.samples.annotated.{AnnPerson, Server}
import morphling.scalacheck.ToGen._
import morphling.scalacheck.annotated.Implicits._
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers

class CirceAnnotatedSpec extends AnyFunSuite with Matchers with EitherValues with ValidatedValues with Checkers {
  private val left = Symbol("left")

  test("A value should serialise to JSON") {
    implicit val encoder = AnnPerson.schema.encoder

    person.asJson shouldBe Json.obj(
      "updateCounter" := 42,
      "roles" := Seq(
        Json.obj(
          "administrator" -> Json.obj(
            "subordinateCount" := 0,
            "department" := "windmill-tilting"
          )
        )
      ),
      "birthDate" := 20147028000L,
      "name" := "Kris Nuttycombe"
    )
  }

  test("A value should be deserialised from JSON") {
    implicit val encoder = AnnPerson.schema.encoder
    val decoder = AnnPerson.schema.decoder

    decoder.decodeJson(person.asJson).value shouldBe person.copy(stamp = 101)
    decoder.decodeAccumulating(person.asJson.hcursor).value shouldBe person.copy(stamp = 101)
  }

  test("Serialization should round-trip values produced by a generator") {
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(AnnPerson.schema.gen)
    implicit val encoder = AnnPerson.schema.encoder
    val decoder = AnnPerson.schema.decoder
    check {
      (p: Person) => decoder.decodeJson(p.asJson).toOption == Some(p)
    }
    check {
      (p: Person) => decoder.decodeAccumulating(p.asJson.hcursor).toOption == Some(p)
    }
  }

  test("Deserialization should fail if some value does not fit limitations") {
    val decoder = Server.schema.decoder

    decoder.decodeJson(Json.obj("host" := "peka.com", "port" := 0)) shouldBe left
    decoder.decodeJson(Json.obj("host" := "peka.com", "port" := 70000)) shouldBe left
  }
}
