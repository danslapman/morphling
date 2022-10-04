package morphling.tapir

import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax.*
import morphling.samples.Person
import morphling.tapir.Implicits.*
import morphling.tapir.ToSchema.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sttp.apispec.openapi.circe.*
import sttp.tapir.docs.apispec.schema.SchemaRenderer

class TapirSchemaSpec extends AnyFunSuite with Matchers with JsonMatchers {
  test("Typeable should be generated") {
    val personTypeableJson = SchemaRenderer.convert(Person.schema.schema).asJson.dropNulls.run

    personTypeableJson should matchJsonString(
      Json
        .obj(
          "required" := "name" :: "birthDate" :: "roles" :: Nil,
          "type" := "object",
          "properties" := Json.obj(
            "name" := Json.obj("type" := "string"),
            "birthDate" := Json.obj("format" := "int64", "type" := "integer"),
            "roles" := Json.obj(
              "type" := "array",
              "items" := Json.obj(
                "oneOf" := Json.arr(
                  Json.obj(
                    "required" := "administrator" :: Nil,
                    "type" := "object",
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
                  ),
                  Json.obj(
                    "required" := "user" :: Nil,
                    "type" := "object",
                    "properties" := Json.obj(
                      "user" := Json.obj(
                        "type" := "object"
                      )
                    )
                  ),
                )
              )
            ),
            "updateCounter" := Json.obj("format" := "int32", "type" := "integer", "default" := 0)
          )
        )
        .spaces2
    )
  }

  test("Flat typeable should be generated") {
    val personTypeableJson = SchemaRenderer.convert(Person.flatSchema.schema).asJson.dropNulls.run

    personTypeableJson should matchJsonString(
      Json
        .obj(
          "required" := "name" :: "birthDate" :: "roles" :: Nil,
          "type" := "object",
          "properties" := Json.obj(
            "name" := Json.obj(
              "type" := "string"
            ),
            "birthDate" := Json.obj(
              "type" := "integer",
              "format" := "int64"
            ),
            "roles" := Json.obj(
              "type" := "array",
              "items" := Json.obj(
                "oneOf" := Json.obj(
                  "required" := "type" :: Nil,
                  "type" := "object",
                  "properties" := Json.obj(
                    "type" := Json.obj(
                      "type" := "string"
                    )
                  )
                ) :: Json.obj(
                  "required" := "type" :: "department" :: "subordinateCount" :: Nil,
                  "type" := "object",
                  "properties" := Json.obj(
                    "type" := Json.obj("type" := "string"),
                    "department" := Json.obj("type" := "string"),
                    "subordinateCount" := Json.obj("type" := "integer", "format" := "int32")
                  )
                ) :: Nil,
                "discriminator" := Json.obj(
                  "propertyName" := "type"
                )
              )
            ),
            "updateCounter" := Json.obj(
              "type" := "integer",
              "format" := "int32",
              "default" := 0
            )
          )
        )
        .spaces2
    )
  }
}
