package morphling.circe.annotated

import io.circe.Json
import io.circe.syntax._
import morphling.circe.ToFilter._
import morphling.circe.ToJson._
import morphling.circe.annotated.Implicits._
import morphling.samples.annotated.AnnPerson
import morphling.samples.person
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class JsonFilterAnnotatedSpec extends AnyFunSuite with Matchers {
  test("Filter should keep correct values as-is") {
    implicit val encoder = AnnPerson.schema.encoder
    val sut = AnnPerson.schema.jsonFilter

    sut(person.asJson) shouldBe Some(person.asJson)
  }

  test("Filter should discard all unrelated data") {
    implicit val encoder = AnnPerson.schema.encoder
    val sut = AnnPerson.schema.jsonFilter

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
}
