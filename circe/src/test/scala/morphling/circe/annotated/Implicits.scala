package morphling.circe.annotated

import cats._
import io.circe.{AccumulatingDecoder, Decoder, Encoder, HCursor}
import morphling.circe.{FromJson, ToJson}
import morphling.protocol.{SArrayT, SBoolT, SCharT, SDoubleT, SFloatT, SIntT, SLongT, SNullT, SStrT}
import morphling.protocol.annotated.SType.ASchema
import morphling.samples.annotated.{NoRestr, Range, Restriction}

object Implicits {
  implicit val annotationValidator: Restriction => HCursor => List[String] = {
    case NoRestr => _ => Nil
    case r @ Range(from, to) => cur =>
      cur.focus.map(fj => fj.asNumber.flatMap(_.toBigDecimal))
        .toRight("Empty cursor")
        .flatMap(_.toRight("Value is not a number"))
        .filterOrElse(_ >= from, s"The value should be gte $from")
        .filterOrElse(_ <= to, s"The value should be lte $to")
        .left.toOption.toList
  }

  implicit def primToJson[A]: ToJson[ASchema[A, ?]] = new ToJson[ASchema[A, ?]] { self =>
    import ToJson._

    val encoder = new (ASchema[A, ?] ~> Encoder) {
      def apply[I](s: ASchema[A, I]): Encoder[I] = s.unmutu match {
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

  implicit def primFromJson[A](implicit vld: A => HCursor => List[String]): FromJson[ASchema[A, ?]] = new FromJson[ASchema[A, ?]] { self =>
    import FromJson._

    val decoder = new (ASchema[A, ?] ~> Decoder) {
      def apply[I](s: ASchema[A, I]): Decoder[I] = s.unmutu match {
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

    val accumulatingDecoder: ASchema[A, ?] ~> AccumulatingDecoder =
      decoder.andThen(λ[Decoder ~> AccumulatingDecoder](AccumulatingDecoder.fromDecoder(_)))
  }
}
