package morphling.tapir

import cats.~>
import morphling.protocol.*
import sttp.tapir.Schema

trait SchemaPack {
  def schemaGen[F[_]: ToSchema]: (SType[F, *] ~> Schema) =
    new (SType[F, *] ~> Schema) {

      import ToSchema.*

      override def apply[I](st: SType[F, I]): Schema[I] = st match {
        case SNullT()   => Schema.schemaForUnit
        case SBoolT()   => Schema.schemaForBoolean
        case SIntT()    => Schema.schemaForInt
        case SLongT()   => Schema.schemaForLong
        case SFloatT()  => Schema.schemaForFloat
        case SDoubleT() => Schema.schemaForDouble
        case SCharT()   => Schema.schemaForString.as[Char]
        case SStrT()    => Schema.schemaForString
        case arr: SArrayT[F, i] =>
          Schema.schemaForIterable[i, Vector](arr.elem.schema).copy(isOptional = false)
      }
    }
}
