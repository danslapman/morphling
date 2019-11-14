package morphling.tschema.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.tschema.{ToTypeable, TypeablePack}
import ru.tinkoff.tschema.swagger.{SwaggerPrimitive, SwaggerTypeable}

object Implicits extends TypeablePack {
  implicit val typeableRestriction: (Restriction ~> λ[T => Endo[SwaggerTypeable[T]]]) =
    new (Restriction ~> λ[T => Endo[SwaggerTypeable[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[SwaggerTypeable[A]] = rs match {
        case Non() => identity
        case Range(from, to) =>
          (typ: SwaggerTypeable[Int]) => typ.updateTyp {
            case SwaggerPrimitive.integer => SwaggerPrimitive.integer.mod(_.copy(minimum = Some(from), maximum = Some(to)))
            case other => other
          }
      }
    }

  implicit val primToTypeable: ToTypeable[ASchema] = new ToTypeable[ASchema] { self =>
    val toTypeable: ASchema ~> SwaggerTypeable = new (ASchema ~> SwaggerTypeable) {
      def apply[A](s: ASchema[A]): SwaggerTypeable[A] = sTypeGen[ASchema[A]#Inner].apply(s.unmutu)
    }
  }
}
