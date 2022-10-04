package morphling.tapir

import cats.free.Trampoline
import cats.instances.vector.*
import cats.syntax.traverse.*
import cats.~>
import io.circe.Json
import morphling.protocol.SType.SSchema
import sttp.tapir.Schema

object Implicits extends SchemaPack {
  implicit val primToSchema: ToSchema[SSchema] = new ToSchema[SSchema] { self =>
    val toSchema: SSchema ~> Schema = new (SSchema ~> Schema) {
      def apply[A](s: SSchema[A]): Schema[A] = schemaGen[SSchema[A]#Inner].apply(s.unmutu)
    }
  }

  implicit class JsonOps(private val json: Json) extends AnyVal {
    def dropNulls: Trampoline[Json] =
      json.arrayOrObject(
        Trampoline.done(json),
        arr => arr.traverse(j => Trampoline.defer(j.dropNulls)).map(Json.fromValues),
        _.filter { case (_, j) => !j.isNull }
          .traverse(obj => Trampoline.defer(obj.dropNulls))
          .map(Json.fromJsonObject)
      )
  }
}
