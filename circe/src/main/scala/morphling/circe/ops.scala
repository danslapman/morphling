package morphling.circe

import cats.data.{NonEmptyList, Validated}
import cats.syntax.either._
import io.circe.{ACursor, AccumulatingDecoder, CursorOp, DecodingFailure}

object ops {
  implicit class AccumulatingDecoderCompanionOps(private val adc: AccumulatingDecoder.type) extends AnyVal {
    def decodeOption[A](ac: AccumulatingDecoder[A]): AccumulatingDecoder[Option[A]] = AccumulatingDecoder.instance { hc =>
      if (hc.value.isNull) Validated.valid(None) else ac.apply(hc).map(Some(_))
    }
  }

  implicit class ACursorOps(private val acursor: ACursor) extends AnyVal {
    @inline private def failNel(ops: List[CursorOp]) = NonEmptyList.one(DecodingFailure("Attempt to decode value on failed cursor", ops))

    def acc[A](implicit adec: AccumulatingDecoder[A]): AccumulatingDecoder.Result[A] =
      acursor.success.toRight(failNel(acursor.history)).flatMap(adec(_).toEither).toValidated
  }
}
