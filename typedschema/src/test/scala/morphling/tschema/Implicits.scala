package morphling.tschema

import cats.~>
import morphling.Schema.Schema
import morphling.json._
import morphling.json.JType.JSchema
import ru.tinkoff.tschema.swagger.SwaggerTypeable

object Implicits {
  implicit val toTypeable: ToTypeable[JSchema] = new ToTypeable[JSchema] { self =>
    override def toTypeable: JSchema ~> SwaggerTypeable = new (JSchema ~> SwaggerTypeable) {
      def apply[A](s: JSchema[A]): SwaggerTypeable[A] = s.unmutu match {
        case JNullT()   => SwaggerTypeable.swaggerTypeableUnit
        case JBoolT()   => SwaggerTypeable.swaggerTypeableBoolean
        case JByteT()   => SwaggerTypeable.swaggerTypeableByte
        case JShortT()  => SwaggerTypeable.swaggerTypeableInteger.as[Short]
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
}
