package morphling.reactivemongo

import cats.~>
import morphling.Schema.Schema
import morphling.protocol.JType.JSchema
import morphling.protocol._
import ops._
import reactivemongo.bson._

object Implicits {
  implicit val primToBson: ToBson[JSchema] = new ToBson[JSchema] { self =>
    override def serialize: JSchema ~> (? => BSONValue) = new (JSchema ~> (? => BSONValue)) {
      override def apply[I](s: JSchema[I]): I => BSONValue = s.unmutu match {
        case JNullT()    => _: I => BSONNull
        case JBoolT()    => BSONBoolean(_)
        case JIntT()     => BSONInteger(_)
        case JLongT()    => BSONLong(_)
        case JFloatT()   => BSONDouble(_)
        case JDoubleT()  => BSONDouble(_)
        case JCharT()    => c => BSONString(c.toString)
        case JStrT()     => BSONString(_)
        case JArrayT(elem) =>
          xs => BSONArray(xs.map(sToB.serialize(elem)).toList)
      }
    }

    val sToB: ToBson[Schema[JSchema, ?]] = ToBson.schemaToBson(self)
  }

  implicit val primFromBson: FromBson[JSchema] = new FromBson[JSchema] { self =>
    val reader = new (JSchema ~> BSONReader[BSONValue, ?]) {
      def apply[I](s: JSchema[I]): BSONReader[BSONValue, I] = s.unmutu match {
        case JNullT()    => BSONReader[BSONValue, I](_ => ())
        case JBoolT()    => bsonBooleanLikeReader.afterRead(_.toBoolean)
        case JIntT()     => bsonNumberLikeReader.afterRead(_.toInt)
        case JLongT()    => bsonNumberLikeReader.afterRead(_.toLong)
        case JFloatT()   => bsonNumberLikeReader.afterRead(_.toFloat)
        case JDoubleT()  => bsonNumberLikeReader.afterRead(_.toDouble)
        case JCharT()    => BSONStringHandler.afterRead(_.head).widenReader
        case JStrT()     => BSONStringHandler.widenReader
        case JArrayT(elem) =>
          BSONReader[BSONArray, I]((arr: BSONArray) => arr.values.map(sFromB.reader(elem).read).toVector).widenReader
      }
    }

    val sFromB: FromBson[Schema[JSchema, ?]] = FromBson.schemaFromBson(self)
  }
}
