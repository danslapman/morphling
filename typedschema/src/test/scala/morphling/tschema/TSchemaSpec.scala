package morphling.tschema

import cats.Eq
import cats.instances.function.*
import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax.*
import morphling.samples.Person
import morphling.tschema.Implicits.*
import morphling.tschema.ToTypeable.*
import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

class TSchemaSpec extends AnyFunSuite with Matchers with JsonMatchers {
  implicit def eqEquality[T: Eq: ClassTag]: Equality[T] =
    (a: T, b: Any) =>
      b match {
        case bt: T => Eq.eqv(a, bt)
        case _     => false
      }

  test("Typeable should be generated") {
    val personTypeableJson = Person.schema.typeable.typ.asJson.dropNulls.run

    personTypeableJson should matchJsonString(
      Json
        .obj(
          "type" := "object",
          "required" := "name" :: "birthDate" :: "roles" :: Nil,
          "properties" := Json.obj(
            "roles" := Json.obj(
              "type" := "array",
              "items" := Json.obj(
                "type" := "object",
                "oneOf" := Json.arr(
                  Json.obj(
                    "type" := "object",
                    "required" := "user" :: Nil,
                    "properties" := Json.obj(
                      "user" := Json.obj(
                        "type" := "object",
                        "properties" := Json.obj()
                      )
                    )
                  ),
                  Json.obj(
                    "type" := "object",
                    "required" := "administrator" :: Nil,
                    "properties" := Json.obj(
                      "administrator" := Json.obj(
                        "type" := "object",
                        "required" := "department" :: "subordinateCount" :: Nil,
                        "properties" := Json.obj(
                          "department" := Json.obj("type" := "string"),
                          "subordinateCount" := Json.obj("format" := "int32", "type" := "integer")
                        )
                      )
                    )
                  )
                )
              )
            ),
            "name" := Json.obj("type" := "string"),
            "birthDate" := Json.obj("format" := "int64", "type" := "integer"),
            "updateCounter" := Json.obj("format" := "int32", "type" := "integer")
          )
        )
        .spaces2
    )
  }

  test("Flat typeable should be generated") {
    val personTypeableJson = Person.flatSchema.typeable.typ.asJson.dropNulls.run

    personTypeableJson should matchJsonString(
      Json
        .obj(
          "type" := "object",
          "required" := "name" :: "birthDate" :: "roles" :: Nil,
          "properties" := Json.obj(
            "roles" := Json.obj(
              "type" := "array",
              "items" := Json.obj(
                "type" := "object",
                "oneOf" := Json.arr(
                  Json.obj(
                    "$ref" := "#/components/schemas/user"
                  ),
                  Json.obj(
                    "$ref" := "#/components/schemas/administrator"
                  )
                ),
                "discriminator" := Json.obj(
                  "propertyName" := "type",
                  "mapping" := Json.obj(
                    "user" := "#/components/schemas/user",
                    "administrator" := "#/components/schemas/administrator"
                  )
                )
              )
            ),
            "name" := Json.obj("type" := "string"),
            "birthDate" := Json.obj("format" := "int64", "type" := "integer"),
            "updateCounter" := Json.obj("format" := "int32", "type" := "integer")
          )
        )
        .spaces2
    )
  }
}
