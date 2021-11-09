package morphling.tschema

import cats.~>
import morphling.protocol.*
import ru.tinkoff.tschema.swagger.SwaggerTypeable

trait TypeablePack {
  def sTypeGen[F[_]: ToTypeable]: (SType[F, *] ~> SwaggerTypeable) =
    new (SType[F, *] ~> SwaggerTypeable) {
      import ToTypeable.*

      override def apply[I](st: SType[F, I]): SwaggerTypeable[I] = st match {
        case SNullT()   => SwaggerTypeable.swaggerTypeableUnit
        case SBoolT()   => SwaggerTypeable.swaggerTypeableBoolean
        case SIntT()    => SwaggerTypeable.swaggerTypeableInteger
        case SLongT()   => SwaggerTypeable.swaggerTypeableLong
        case SFloatT()  => SwaggerTypeable.swaggerTypeableFloat
        case SDoubleT() => SwaggerTypeable.swaggerTypeableDouble
        case SCharT()   => SwaggerTypeable.swaggerTypeableString.as[Char]
        case SStrT()    => SwaggerTypeable.swaggerTypeableString
        case arr: SArrayT[F, i] =>
          SwaggerTypeable.swaggerVectorTypeable(arr.elem.typeable)
      }
    }
}
