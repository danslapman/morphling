package morphling.reactivemongo.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.reactivemongo.{FromBson, ReactivemongoPack, ToBson}
import reactivemongo.api.bson.*

object Implicits extends ReactivemongoPack {
  implicit val readerRestrictions: (Restriction ~> λ[T => Endo[BSONReader[T]]]) =
    new (Restriction ~> λ[T => Endo[BSONReader[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[BSONReader[A]] = rs match {
        case Non() => identity
        case Range(from, to) =>
          (rdr: BSONReader[Int]) =>
            rdr
              .afterRead(i => i.ensuring(i > from, s"Value should be greater than $from"))
              .afterRead(i => i.ensuring(i < to, s"Value should be less than $to"))
      }
    }

  implicit val primToBson: ToBson[ASchema] = new ToBson[ASchema] { self =>
    val writer: ASchema ~> BSONWriter = new (ASchema ~> BSONWriter) {
      override def apply[I](s: ASchema[I]): BSONWriter[I] =
        sTypeWriter[ASchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromBson: FromBson[ASchema] = new FromBson[ASchema] { self =>
    val reader = new (ASchema ~> BSONReader) {
      def apply[I](s: ASchema[I]): BSONReader[I] =
        sTypeReader[ASchema[I]#Inner].apply(s.unmutu)
    }
  }
}
