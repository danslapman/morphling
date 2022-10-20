package morphling.tapir.annotated

import com.stephenn.scalatest.circe.JsonMatchers
import io.circe.Json
import io.circe.syntax.*
import morphling.samples.annotated.Server
import morphling.tapir.Implicits.JsonOps
import morphling.tapir.ToSchema.*
import morphling.tapir.annotated.Implicits.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sttp.apispec.openapi.circe.*
import sttp.tapir.docs.apispec.schema.SchemaRenderer

class TapirSchemaAnnotatedSpec extends AnyFunSuite with Matchers with JsonMatchers {
  test("Annotated typeable should contain restrictions") {
    val serverTypeableJson = SchemaRenderer.convert(Server.schema.schema).asJson.dropNulls.run

    serverTypeableJson should matchJsonString(
      Json
        .obj(
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
        )
        .spaces2
    )
  }
}
