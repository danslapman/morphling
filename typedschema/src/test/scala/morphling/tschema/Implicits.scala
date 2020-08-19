package morphling.tschema

import cats.free.Trampoline
import cats.instances.vector._
import cats.syntax.traverse._
import cats.~>
import io.circe.Json
import morphling.protocol.SType.SSchema
import ru.tinkoff.tschema.swagger.SwaggerTypeable

object Implicits extends TypeablePack {
  implicit val primToTypeable: ToTypeable[SSchema] = new ToTypeable[SSchema] { self =>
    val toTypeable: SSchema ~> SwaggerTypeable = new (SSchema ~> SwaggerTypeable) {
      def apply[A](s: SSchema[A]): SwaggerTypeable[A] = sTypeGen[SSchema[A]#Inner].apply(s.unmutu)
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