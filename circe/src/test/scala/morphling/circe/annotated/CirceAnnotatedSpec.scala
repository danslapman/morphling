package morphling.circe.annotated

import cats.scalatest.ValidatedValues
import io.circe.Json
import io.circe.syntax._
import morphling.circe.FromJson._
import morphling.circe.ToJson._
import morphling.circe.annotated.Implicits._
import morphling.samples.annotated.AnnPerson
import morphling.samples.person
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
}
