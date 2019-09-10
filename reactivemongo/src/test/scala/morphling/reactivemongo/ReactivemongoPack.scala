package morphling.reactivemongo

import cats.~>
import morphling.protocol._
import ops._
import reactivemongo.bson._

trait ReactivemongoPack {
  def sTypeWriter[F[_]: ToBson]: (SType[F, *] ~> BSONWriter[*, BSONValue]) =
    new (SType[F, *] ~> BSONWriter[*, BSONValue]) {
      import ToBson._

      override def apply[I](st: SType[F, I]): BSONWriter[I, BSONValue] = st match {
        case SNullT() => _: I => BSONNull
        case SBoolT() => BSONBoolean(_)
        case SIntT() => BSONInteger(_)
        case SLongT() => BSONLong(_)
        case SFloatT() => BSONDouble(_)
        case SDoubleT() => BSONDouble(_)
        case SCharT() => c => BSONString(c.toString)
        case SStrT() => BSONString(_)
        case SArrayT(elem) =>
          xs => BSONArray(xs.map(elem.writer.write).toList)
      }
    }

  def sTypeReader[F[_]: FromBson]: (SType[F, *] ~> BSONReader[BSONValue, *]) =
    new (SType[F, *] ~> BSONReader[BSONValue, *]) {
      import FromBson._

      override def apply[I](st: SType[F, I]): BSONReader[BSONValue, I] = st match {
        case SNullT()    => BSONReader[BSONValue, I](_ => ())
        case SBoolT()    => bsonBooleanLikeReader.afterRead(_.toBoolean)
        case SIntT()     => bsonNumberLikeReader.afterRead(_.toInt)
        case SLongT()    => bsonNumberLikeReader.afterRead(_.toLong)
        case SFloatT()   => bsonNumberLikeReader.afterRead(_.toFloat)
        case SDoubleT()  => bsonNumberLikeReader.afterRead(_.toDouble)
        case SCharT()    => BSONStringHandler.afterRead(_.head).widenReader
        case SStrT()     => BSONStringHandler.widenReader
        case sa: SArrayT[F, i] =>
          BSONReader[BSONArray, I]((arr: BSONArray) => arr.values.map(sa.elem.reader.read).toVector).widenReader
      }
    }
}
