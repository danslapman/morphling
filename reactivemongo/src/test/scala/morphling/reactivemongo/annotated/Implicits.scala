package morphling.reactivemongo.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.reactivemongo.{FromBson, ReactivemongoPack, ToBson}
import reactivemongo.bson._

object Implicits extends ReactivemongoPack {
  implicit val readerRestrictions: (Restriction ~> λ[T => Endo[BSONReader[BSONValue, T]]]) =
    new (Restriction ~> λ[T => Endo[BSONReader[BSONValue, T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[BSONReader[BSONValue, A]] = rs match {
        case Non => identity
        case Range(from, to) =>
          (rdr: BSONReader[BSONValue, Int]) => rdr
              .afterRead(i => i.ensuring(i > from, s"Value should be greater than $from"))
            .afterRead(i => i.ensuring(i < to, s"Value should be less than $to"))
      }
    }

  implicit val primToBson: ToBson[ASchema] = new ToBson[ASchema] { self =>
    val writer: ASchema ~> BSONWriter[*, BSONValue] = new (ASchema ~> BSONWriter[*, BSONValue]) {
      override def apply[I](s: ASchema[I]): BSONWriter[I, BSONValue] =
        sTypeWriter[ASchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromBson: FromBson[ASchema] = new FromBson[ASchema] { self =>
    val reader = new (ASchema ~> BSONReader[BSONValue, *]) {
      def apply[I](s: ASchema[I]): BSONReader[BSONValue, I] =
        sTypeReader[ASchema[I]#Inner].apply(s.unmutu)
    }
  }
}
