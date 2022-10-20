package morphling.tapir.annotated

import cats.Endo
import cats.~>
import morphling.protocol.annotated.Non
import morphling.protocol.annotated.Range
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.tapir.SchemaPack
import morphling.tapir.ToSchema
import sttp.tapir.Schema
import sttp.tapir.Validator

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
