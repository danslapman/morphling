package morphling.circe

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import morphling.circe.Implicits.*
import morphling.circe.ToFilter.*
import morphling.circe.ToJson.*
import morphling.samples.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonFilterSpec extends AnyFunSuite with Matchers {
  test("Filter should keep correct values as-is") {
    implicit val encoder: Encoder[Person] = Person.schema.encoder
    val sut                               = Person.schema.jsonFilter

    sut(person.asJson) shouldBe Some(person.asJson)
  }

  test("Filter should discard all unrelated data") {
    implicit val encoder: Encoder[Person] = Person.schema.encoder
    val sut                               = Person.schema.jsonFilter

    val json = Json.obj(
      "updateCounter" := 42,
      "roles" := Seq(
        Json.obj(
          "administrator" -> Json.obj(
            "subordinateCount" := 0,
            "department" := "windmill-tilting",
            "foo" := "bar"
          ),
          "val" := 42
        )
      ),
      "birthDate" := 20147028000L,
      "name" := "Kris Nuttycombe",
      "peka" := "yoba"
    )

    sut(json) shouldBe Some(person.asJson)
  }

  test("Filter should discard all unrelated data with flat schema") {
    implicit val encoder: Encoder[Person] = Person.flatSchema.encoder
    val sut                               = Person.flatSchema.jsonFilter

    val json = Json.obj(
      "updateCounter" := 42,
      "roles" := Seq(
        Json.obj(
          "type" := "administrator",
          "subordinateCount" := 0,
          "department" := "windmill-tilting",
          "val" := 42
        )
      ),
      "birthDate" := 20147028000L,
      "name" := "Kris Nuttycombe",
      "peka" := "yoba"
    )

    sut(json) shouldBe Some(person.asJson)
  }
}
