package morphling.circe

import io.circe.syntax._
import morphling.circe.Implicits._
import morphling.circe.ToFilter._
import morphling.circe.ToJson._
import morphling.samples._
import org.scalatest.{FunSuite, Matchers}

class JsonFilterSpec extends FunSuite with Matchers {
  test("Filter should keep correct values as-is") {
    implicit val encoder = Person.schema.encoder
    val sut = Person.schema.jsonFilter

    sut(person.asJson) shouldBe Some(person.asJson)
  }
}
