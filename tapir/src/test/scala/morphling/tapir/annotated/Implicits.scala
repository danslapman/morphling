package morphling.tapir.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.tapir.{SchemaPack, ToSchema}
import sttp.tapir.{Schema, Validator}

object Implicits extends SchemaPack {
  implicit val schemaRestriction: (Restriction ~> λ[T => Endo[Schema[T]]]) =
    new (Restriction ~> λ[T => Endo[Schema[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[Schema[A]] = rs match {
        case Non() => identity
        case Range(from, to) =>
          (sch: Schema[Int]) => sch.validate(Validator.min(from).and(Validator.max(to)))
      }
    }

  implicit val primToSchema: ToSchema[ASchema] = new ToSchema[ASchema] { self =>
    val toSchema: ASchema ~> Schema = new (ASchema ~> Schema) {
      def apply[A](s: ASchema[A]): Schema[A] = schemaGen[ASchema[A]#Inner].apply(s.unmutu)
    }
  }
}
