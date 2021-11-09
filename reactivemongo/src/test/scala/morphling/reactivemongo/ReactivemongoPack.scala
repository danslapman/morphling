package morphling.reactivemongo

import scala.util.Success

import cats.instances.try_.*
import cats.instances.vector.*
import cats.syntax.all.*
import cats.~>
import morphling.protocol.*
import reactivemongo.api.bson.*

trait ReactivemongoPack {
  def sTypeWriter[F[_]: ToBson]: (SType[F, *] ~> BSONWriter) =
    new (SType[F, *] ~> BSONWriter) {
      import ToBson.*

      override def apply[I](st: SType[F, I]): BSONWriter[I] = {
        st match {
          case SNullT() => _: I => Success(BSONNull)
          case SBoolT() => b => Success(BSONBoolean(b))
          case SIntT() => i => Success(BSONInteger(i))
          case SLongT() => l => Success(BSONLong(l))
          case SFloatT() => f => Success(BSONDouble(f))
          case SDoubleT() => d => Success(BSONDouble(d))
          case SCharT() => c => Success(BSONString(c.toString))
          case SStrT() => s => Success(BSONString(s))
          case sa: SArrayT[F, i] =>
            (xs: Vector[i]) => xs.traverse(sa.elem.writer.writeTry).map(BSONArray(_))
        }
      }
    }

  def sTypeReader[F[_]: FromBson]: (SType[F, *] ~> BSONReader) =
    new (SType[F, *] ~> BSONReader) {
      import FromBson.*

      override def apply[I](st: SType[F, I]): BSONReader[I] = st match {
        case SNullT()    => BSONReader[I](_ => ())
        case SBoolT()    => BSONBooleanLike.Handler.readTry(_).flatMap(_.toBoolean)
        case SIntT()     => BSONNumberLike.Handler.readTry(_).flatMap(_.toInt)
        case SLongT()    => BSONNumberLike.Handler.readTry(_).flatMap(_.toLong)
        case SFloatT()   => BSONNumberLike.Handler.readTry(_).flatMap(_.toFloat)
        case SDoubleT()  => BSONNumberLike.Handler.readTry(_).flatMap(_.toDouble)
        case SCharT()    => BSONStringHandler.afterRead(_.head)
        case SStrT()     => BSONStringHandler
        case sa: SArrayT[F, i] =>
          BSONReader.collect { case arr: BSONArray => arr }.readTry(_).flatMap { arr =>
            arr.values.toVector.traverse(sa.elem.reader.readTry)
          }
      }
    }
}
