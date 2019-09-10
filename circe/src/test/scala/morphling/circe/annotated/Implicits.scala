package morphling.circe.annotated

import cats._
import io.circe.{AccumulatingDecoder, Decoder, Encoder}
import morphling.circe.{FromJson, ToJson}
import morphling.protocol.{SArrayT, SBoolT, SCharT, SDoubleT, SFloatT, SIntT, SLongT, SNullT, SStrT}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.protocol.annotated.{Non, Range, Restriction}

object Implicits {
  implicit val decoderRestriction: (Restriction ~> λ[T => Endo[Decoder[T]]]) =
    new (Restriction ~> λ[T => Endo[Decoder[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[Decoder[A]] = rs match {
        case Non => identity
        case Range(from, to) =>
          (dec: Decoder[Int]) => dec
            .ensure(_ > from, s"Value should be greater than $from")
            .ensure(_ < to, s"Value should be less than $to")
      }
    }

  implicit val primToJson: ToJson[ASchema] = new ToJson[ASchema] { self =>
    import ToJson._

    val encoder = new (ASchema ~> Encoder) {
      def apply[I](s: ASchema[I]): Encoder[I] = s.unmutu match {
        case _: SNullT[s.Inner]     => Encoder.encodeUnit
        case _: SBoolT[s.Inner]     => Encoder.encodeBoolean
        case _: SIntT[s.Inner]      => Encoder.encodeInt
        case _: SLongT[s.Inner]     => Encoder.encodeLong
        case _: SFloatT[s.Inner]    => Encoder.encodeFloat
        case _: SDoubleT[s.Inner]   => Encoder.encodeDouble
        case _: SCharT[s.Inner]     => Encoder.encodeChar
        case _: SStrT[s.Inner]      => Encoder.encodeString
        case a: SArrayT[s.Inner, i] => Encoder.encodeVector(a.elem.encoder)
      }
    }
  }

  implicit val primFromJson: FromJson[ASchema] = new FromJson[ASchema] { self =>
    import FromJson._

    val decoder = new (ASchema ~> Decoder) {
      def apply[I](s: ASchema[I]): Decoder[I] = s.unmutu match {
        case _: SNullT[s.Inner]     => Decoder.decodeUnit
        case _: SBoolT[s.Inner]     => Decoder.decodeBoolean
        case _: SIntT[s.Inner]      => Decoder.decodeInt
        case _: SLongT[s.Inner]     => Decoder.decodeLong
        case _: SFloatT[s.Inner]    => Decoder.decodeFloat
        case _: SDoubleT[s.Inner]   => Decoder.decodeDouble
        case _: SCharT[s.Inner]     => Decoder.decodeChar
        case _: SStrT[s.Inner]      => Decoder.decodeString
        case a: SArrayT[s.Inner, i] => Decoder.decodeVector(a.elem.decoder)
      }
    }

    val accumulatingDecoder: ASchema ~> AccumulatingDecoder =
      decoder.andThen(λ[Decoder ~> AccumulatingDecoder](AccumulatingDecoder.fromDecoder(_)))
  }
}
