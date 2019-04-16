package morphling.circe

import cats._
import io.circe.{Decoder, Encoder}
import morphling.protocol._
import morphling.protocol.SType.SSchema

object Implicits {
  implicit val primToJson: ToJson[SSchema] = new ToJson[SSchema] { self =>
    import ToJson._

    val encoder = new (SSchema ~> Encoder) {
      def apply[I](s: SSchema[I]): Encoder[I] = s.unmutu match {
        case SNullT()    => Encoder.encodeUnit
        case SBoolT()    => Encoder.encodeBoolean
        case SIntT()     => Encoder.encodeInt
        case SLongT()    => Encoder.encodeLong
        case SFloatT()   => Encoder.encodeFloat
        case SDoubleT()  => Encoder.encodeDouble
        case SCharT()    => Encoder.encodeChar
        case SStrT()     => Encoder.encodeString
        case SArrayT(elem) => Encoder.encodeVector(elem.encoder)
      }
    }
  }

  implicit val primFromJson: FromJson[SSchema] = new FromJson[SSchema] { self =>
    import FromJson._

    val decoder = new (SSchema ~> Decoder) {
      def apply[I](s: SSchema[I]): Decoder[I] = s.unmutu match {
        case SNullT()    => Decoder.decodeUnit
        case SBoolT()    => Decoder.decodeBoolean
        case SIntT()     => Decoder.decodeInt
        case SLongT()    => Decoder.decodeLong
        case SFloatT()   => Decoder.decodeFloat
        case SDoubleT()  => Decoder.decodeDouble
        case SCharT()    => Decoder.decodeChar
        case SStrT()     => Decoder.decodeString
        case SArrayT(elem) => Decoder.decodeVector(elem.decoder)
      }
    }
  }
}
