package morphling.tschema

import cats.~>
import cats.free.Trampoline
import cats.instances.vector._
import cats.syntax.traverse._
import io.circe.Json
import morphling.Schema.Schema
import morphling.protocol._
import morphling.protocol.JType.JSchema
import ru.tinkoff.tschema.swagger.SwaggerTypeable

object Implicits {
  implicit val toTypeable: ToTypeable[JSchema] = new ToTypeable[JSchema] { self =>
    override def toTypeable: JSchema ~> SwaggerTypeable = new (JSchema ~> SwaggerTypeable) {
      def apply[A](s: JSchema[A]): SwaggerTypeable[A] = s.unmutu match {
        case JNullT()   => SwaggerTypeable.swaggerTypeableUnit
        case JBoolT()   => SwaggerTypeable.swaggerTypeableBoolean
        case JIntT()    => SwaggerTypeable.swaggerTypeableInteger
        case JLongT()   => SwaggerTypeable.swaggerTypeableLong
        case JFloatT()  => SwaggerTypeable.swaggerTypeableFloat
        case JDoubleT() => SwaggerTypeable.swaggerTypeableDouble
        case JCharT()   => SwaggerTypeable.swaggerTypeableString.as[Char]
        case JStrT()    => SwaggerTypeable.swaggerTypeableString
        case arr: JArrayT[Schema[JSchema, ?], i] =>
          val baseTyp: SwaggerTypeable[i] =
            ToTypeable.schemaToTypeable[JSchema](self).toTypeable(arr.elem)
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