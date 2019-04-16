package morphling.reactivemongo

import cats.~>
import morphling.protocol.SType.SSchema
import morphling.protocol._
import ops._
import reactivemongo.bson._

object Implicits {
  implicit val primToBson: ToBson[SSchema] = new ToBson[SSchema] { self =>
    import ToBson._

    override def writer: SSchema ~> BSONWriter[?, BSONValue] = new (SSchema ~> BSONWriter[?, BSONValue]) {
      override def apply[I](s: SSchema[I]): BSONWriter[I, BSONValue] = s.unmutu match {
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

  implicit val primFromBson: FromBson[SSchema] = new FromBson[SSchema] { self =>
    import FromBson._

    val reader = new (SSchema ~> BSONReader[BSONValue, ?]) {
      def apply[I](s: SSchema[I]): BSONReader[BSONValue, I] = s.unmutu match {
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
