package morphling.reactivemongo

import reactivemongo.bson._

import scala.util.{Failure, Success, Try}

object ops {
  implicit class BSONReaderExts[BV <: BSONValue, T](private val rdr: BSONReader[BV, T]) extends AnyVal {
    def widenReader: BSONReader[BSONValue, T] =
      new BSONReader[BSONValue, T] {
        override def readTry(value: BSONValue): Try[T] =
          Try(value.asInstanceOf[BV]) match {
            case Failure(_) =>
              Failure(
                exceptions.TypeDoesNotMatch(
                  s"Cannot convert $value: ${value.getClass} with ${rdr.getClass}"
                )
              )

            case Success(bson) => rdr.readTry(bson)
          }

        override def read(bson: BSONValue): T = readTry(bson).get
      }
  }
}
