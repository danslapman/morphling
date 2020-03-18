package morphling.circe

import io.circe.Json
import io.circe.syntax._
import morphling.circe.Implicits._
import morphling.circe.ToFilter._
import morphling.circe.ToJson._
import morphling.samples._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonFilterSpec extends AnyFunSuite with Matchers {
  test("Filter should keep correct values as-is") {
    implicit val encoder = Person.schema.encoder
    val sut = Person.schema.jsonFilter

    sut(person.asJson) shouldBe Some(person.asJson)
  }

  test("Filter should discard all unrelated data") {
    implicit val encoder = Person.schema.encoder
    val sut = Person.schema.jsonFilter

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
    implicit val encoder = Person.flatSchema.encoder
    val sut = Person.flatSchema.jsonFilter

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
