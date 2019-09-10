package morphling.circe.annotated

import cats.scalatest.ValidatedValues
import io.circe.Json
import io.circe.syntax._
import morphling.circe.FromJson._
import morphling.circe.ToJson._
import morphling.circe.annotated.Implicits._
import morphling.samples.annotated.AnnPerson
import morphling.samples.{Person, person}
import morphling.scalacheck.annotated.Implicits._
import morphling.scalacheck.ToGen._
import org.scalacheck.Arbitrary
import org.scalatest.{EitherValues, FunSuite, Matchers}
import org.scalatestplus.scalacheck.Checkers

class CirceAnnotatedSpec extends FunSuite with Matchers with EitherValues with ValidatedValues with Checkers {
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

  test("A value should be deserialised from JSON"){
    implicit val encoder = AnnPerson.schema.encoder
    val decoder = AnnPerson.schema.decoder
    val accDecoder = AnnPerson.schema.accumulatingDecoder

    decoder.decodeJson(person.asJson).right.value shouldBe person.copy(stamp = 101)
    accDecoder.apply(person.asJson.hcursor).value shouldBe person.copy(stamp = 101)
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(AnnPerson.schema.gen)
    implicit val encoder = AnnPerson.schema.encoder
    val decoder = AnnPerson.schema.decoder
    val accDecoder = AnnPerson.schema.accumulatingDecoder
    check {
      (p: Person) => decoder.decodeJson(p.asJson).toOption == Some(p)
    }
    check {
      (p: Person) => accDecoder(p.asJson.hcursor).toOption == Some(p)
    }
  }
}
