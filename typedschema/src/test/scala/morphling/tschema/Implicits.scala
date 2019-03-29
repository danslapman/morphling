package morphling.tschema

import cats.~>
import cats.free.Trampoline
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.Json
import morphling.Schema.Schema
import morphling.protocol._
import morphling.protocol.SType.SSchema
import ru.tinkoff.tschema.swagger.SwaggerTypeable

object Implicits {
  implicit val toTypeable: ToTypeable[SSchema] = new ToTypeable[SSchema] { self =>
    override def toTypeable: SSchema ~> SwaggerTypeable = new (SSchema ~> SwaggerTypeable) {
      def apply[A](s: SSchema[A]): SwaggerTypeable[A] = s.unmutu match {
        case SNullT()   => SwaggerTypeable.swaggerTypeableUnit
        case SBoolT()   => SwaggerTypeable.swaggerTypeableBoolean
        case SIntT()    => SwaggerTypeable.swaggerTypeableInteger
        case SLongT()   => SwaggerTypeable.swaggerTypeableLong
        case SFloatT()  => SwaggerTypeable.swaggerTypeableFloat
        case SDoubleT() => SwaggerTypeable.swaggerTypeableDouble
        case SCharT()   => SwaggerTypeable.swaggerTypeableString.as[Char]
        case SStrT()    => SwaggerTypeable.swaggerTypeableString
        case arr: SArrayT[Schema[SSchema, ?], i] =>
          val baseTyp: SwaggerTypeable[i] =
            ToTypeable.schemaToTypeable[SSchema](self).toTypeable(arr.elem)
          SwaggerTypeable.swaggerVectorTypeable(baseTyp)
      }
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