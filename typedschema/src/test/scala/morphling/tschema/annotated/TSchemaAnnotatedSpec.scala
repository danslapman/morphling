package morphling.tschema.annotated

import cats.Eq
import cats.instances.function._
import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax._
import morphling.samples.annotated.Server
import morphling.tschema.ToTypeable._
import morphling.tschema.Implicits.JsonOps
import morphling.tschema.annotated.Implicits._
import org.scalactic.Equality
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.ClassTag

class TSchemaAnnotatedSpec extends FunSuite with Matchers with JsonMatchers {
  implicit def eqEquality[T: Eq : ClassTag]: Equality[T] =
    (a: T, b: Any) => b match {
      case bt: T => Eq.eqv(a, bt)
      case _ => false
    }

  test("Annotated typeable should contain restrictions") {
    val serverTypeable = Server.schema.typeable.typ.asJson.dropNulls.run

    serverTypeable should matchJsonString(Json.obj(
      "type" := "object",
      "required" := "host" :: "port" :: Nil,
      "properties" := Json.obj(
        "host" := Json.obj(
          "type" := "string"
        ),
        "port" := Json.obj(
          "format" := "int32",
          "maximum" := 65535,
          "minimum" := 1,
          "type" := "integer"
        )
      )
    ).spaces2)
  }
}