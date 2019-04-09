package morphling.tschema

import cats.Eq
import cats.instances.function._
import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax._
import morphling.samples.Person
import morphling.tschema.Implicits._
import morphling.tschema.ToTypeable._
import org.scalactic.Equality
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.ClassTag

class TSchemaSpec extends FunSuite with Matchers with JsonMatchers {
  implicit def eqEquality[T: Eq : ClassTag]: Equality[T] =
    (a: T, b: Any) => b match {
      case bt: T => Eq.eqv(a, bt)
      case _ => false
    }

  test("Typeable should be generated") {
    val personTypeableJson = Person.schema.toTypeable.typ.asJson.dropNulls.run

    personTypeableJson should matchJsonString(Json.obj(
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
                    "required" := List.empty[String],
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
    ).spaces2)
  }

  test("Flat typeable should be generated") {
    val personTypeableJson = Person.flatSchema.toTypeable.typ.asJson.dropNulls.run

    personTypeableJson should matchJsonString(Json.obj(
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
                "required" := "type" :: Nil,
                "properties" := Json.obj(
                  "type" := Json.obj("type" := "string")
                )
              ),
              Json.obj(
                "type" := "object",
                "required" :=  "department" :: "subordinateCount" :: "type" :: Nil,
                "properties" := Json.obj(
                  "type" := Json.obj("type" := "string"),
                  "department" := Json.obj("type" := "string"),
                  "subordinateCount" := Json.obj("format" := "int32", "type" := "integer")
                )
              )
            ),
            "discriminator" := Json.obj(
              "propertyName" := "type",
              "mapping" := Json.obj()
            )
          )
        ),
        "name" := Json.obj("type" := "string"),
        "birthDate" := Json.obj("format" := "int64", "type" := "integer"),
        "updateCounter" := Json.obj("format" := "int32", "type" := "integer")
      )
    ).spaces2)
  }
}
