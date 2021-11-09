package morphling.tschema.annotated

import scala.reflect.ClassTag

import cats.Eq
import cats.instances.function.*
import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax.*
import morphling.samples.annotated.Server
import morphling.tschema.Implicits.JsonOps
import morphling.tschema.ToTypeable.*
import morphling.tschema.annotated.Implicits.*
import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TSchemaAnnotatedSpec extends AnyFunSuite with Matchers with JsonMatchers {
  implicit def eqEquality[T: Eq : ClassTag]: Equality[T] =
    (a: T, b: Any) => b match {
      case bt: T => Eq.eqv(a, bt)
      case _ => false
    }

  test("Annotated typeable should contain restrictions") {
    val serverTypeable = Server.schema.typeable
    val serverTypeableJson = serverTypeable.typ.asJson.dropNulls.run

    serverTypeableJson should matchJsonString(Json.obj(
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
