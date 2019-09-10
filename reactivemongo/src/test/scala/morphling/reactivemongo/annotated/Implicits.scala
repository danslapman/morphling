package morphling.reactivemongo.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.protocol.{SArrayT, SBoolT, SCharT, SDoubleT, SFloatT, SIntT, SLongT, SNullT, SStrT}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.reactivemongo.{FromBson, ToBson}
import morphling.reactivemongo.ops._
import reactivemongo.bson._

object Implicits {
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
    import ToBson._

    val writer: ASchema ~> BSONWriter[*, BSONValue] = new (ASchema ~> BSONWriter[*, BSONValue]) {
      override def apply[I](s: ASchema[I]): BSONWriter[I, BSONValue] = s.unmutu match {
        case SNullT()    => _: I => BSONNull
        case SBoolT()    => BSONBoolean(_)
        case SIntT()     => BSONInteger(_)
        case SLongT()    => BSONLong(_)
        case SFloatT()   => BSONDouble(_)
        case SDoubleT()  => BSONDouble(_)
        case SCharT()    => c => BSONString(c.toString)
        case SStrT()     => BSONString(_)
        case SArrayT(elem) =>
          xs => BSONArray(xs.map(elem.writer.write).toList)
      }
    }
  }

  implicit val primFromBson: FromBson[ASchema] = new FromBson[ASchema] { self =>
    import FromBson._

    val reader = new (ASchema ~> BSONReader[BSONValue, *]) {
      def apply[I](s: ASchema[I]): BSONReader[BSONValue, I] = s.unmutu match {
        case SNullT()    => BSONReader[BSONValue, I](_ => ())
        case SBoolT()    => bsonBooleanLikeReader.afterRead(_.toBoolean)
        case SIntT()     => bsonNumberLikeReader.afterRead(_.toInt)
        case SLongT()    => bsonNumberLikeReader.afterRead(_.toLong)
        case SFloatT()   => bsonNumberLikeReader.afterRead(_.toFloat)
        case SDoubleT()  => bsonNumberLikeReader.afterRead(_.toDouble)
        case SCharT()    => BSONStringHandler.afterRead(_.head).widenReader
        case SStrT()     => BSONStringHandler.widenReader
        case SArrayT(elem) =>
          BSONReader[BSONArray, I]((arr: BSONArray) => arr.values.map(elem.reader.read).toVector).widenReader
      }
    }
  }
}
