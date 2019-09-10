package morphling.tschema.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.protocol.{SArrayT, SBoolT, SCharT, SDoubleT, SFloatT, SIntT, SLongT, SNullT, SStrT}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.tschema.ToTypeable
import ru.tinkoff.tschema.swagger.{SwaggerPrimitive, SwaggerTypeable}

object Implicits {
  implicit val typeableRestriction: (Restriction ~> λ[T => Endo[SwaggerTypeable[T]]]) =
    new (Restriction ~> λ[T => Endo[SwaggerTypeable[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[SwaggerTypeable[A]] = rs match {
        case Non => identity
        case Range(from, to) =>
          (typ: SwaggerTypeable[Int]) => typ.updateTyp {
            case SwaggerPrimitive.integer => SwaggerPrimitive.integer.mod(_.copy(minimum = Some(from), maximum = Some(to)))
            case other => other
          }
      }
    }

  implicit val toTypeable: ToTypeable[ASchema] = new ToTypeable[ASchema] { self =>
    val toTypeable: ASchema ~> SwaggerTypeable = new (ASchema ~> SwaggerTypeable) {
      def apply[A](s: ASchema[A]): SwaggerTypeable[A] = s.unmutu match {
        case SNullT()   => SwaggerTypeable.swaggerTypeableUnit
        case SBoolT()   => SwaggerTypeable.swaggerTypeableBoolean
        case SIntT()    => SwaggerTypeable.swaggerTypeableInteger
        case SLongT()   => SwaggerTypeable.swaggerTypeableLong
        case SFloatT()  => SwaggerTypeable.swaggerTypeableFloat
        case SDoubleT() => SwaggerTypeable.swaggerTypeableDouble
        case SCharT()   => SwaggerTypeable.swaggerTypeableString.as[Char]
        case SStrT()    => SwaggerTypeable.swaggerTypeableString
        case arr: SArrayT[s.Inner, i] =>
          val baseTyp: SwaggerTypeable[i] =
            ToTypeable.annSchemaToTypeable[ASchema, Restriction](self, typeableRestriction).toTypeable(arr.elem)
          SwaggerTypeable.swaggerVectorTypeable(baseTyp)
      }
    }
  }
}
