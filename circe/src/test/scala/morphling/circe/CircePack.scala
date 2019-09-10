package morphling.circe

import cats.~>
import io.circe.{Decoder, Encoder}
import morphling.protocol._

trait CircePack {
  def sTypeEncoder[F[_]: ToJson]: (SType[F, *] ~> Encoder) =
    new (SType[F, *] ~> Encoder) {
      import ToJson._

      override def apply[A](st: SType[F, A]): Encoder[A] = st match {
        case SNullT() => Encoder.encodeUnit
        case SBoolT() => Encoder.encodeBoolean
        case SIntT() => Encoder.encodeInt
        case SLongT() => Encoder.encodeLong
        case SFloatT() => Encoder.encodeFloat
        case SDoubleT() => Encoder.encodeDouble
        case SCharT() => Encoder.encodeChar
        case SStrT() => Encoder.encodeString
        case SArrayT(elem) => Encoder.encodeVector(elem.encoder)
      }
    }

  def sTypeDecoder[F[_]: FromJson]: (SType[F, *] ~> Decoder) =
    new (SType[F, *] ~> Decoder) {
      import FromJson._

      override def apply[A](st: SType[F, A]): Decoder[A] = st match {
        case SNullT() => Decoder.decodeUnit
        case SBoolT() => Decoder.decodeBoolean
        case SIntT() => Decoder.decodeInt
        case SLongT() => Decoder.decodeLong
        case SFloatT() => Decoder.decodeFloat
        case SDoubleT() => Decoder.decodeDouble
        case SCharT() => Decoder.decodeChar
        case SStrT() => Decoder.decodeString
        case SArrayT(elem) => Decoder.decodeVector(elem.decoder)
      }
    }
}
