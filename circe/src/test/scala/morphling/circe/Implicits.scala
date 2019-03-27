package morphling.circe

import cats._
import io.circe.{Decoder, Encoder, Json}
import morphling.protocol._
import morphling.protocol.JType.JSchema
import morphling.Schema.Schema

object Implicits {
  implicit val primToJson: ToJson[JSchema] = new ToJson[JSchema] { self =>
    val encoder = new (JSchema ~> Encoder) {
      def apply[I](s: JSchema[I]): Encoder[I] = s.unmutu match {
        case JNullT()    => Encoder.encodeUnit
        case JBoolT()    => Encoder.encodeBoolean
        case JIntT()     => Encoder.encodeInt
        case JLongT()    => Encoder.encodeLong
        case JFloatT()   => Encoder.encodeFloat
        case JDoubleT()  => Encoder.encodeDouble
        case JCharT()    => Encoder.encodeChar
        case JStrT()     => Encoder.encodeString
        case JArrayT(elem) =>
          xs => Json.fromValues(xs.map(sToJ.encoder(elem)(_)).toList)
      }
    }

    val sToJ: ToJson[Schema[JSchema, ?]] = ToJson.schemaToJson(self)
  }

  implicit val primFromJson: FromJson[JSchema] = new FromJson[JSchema] { self =>
    val decoder = new (JSchema ~> Decoder) {
      def apply[I](s: JSchema[I]): Decoder[I] = s.unmutu match {
        case JNullT()    => Decoder.decodeUnit
        case JBoolT()    => Decoder.decodeBoolean
        case JIntT()     => Decoder.decodeInt
        case JLongT()    => Decoder.decodeLong
        case JFloatT()   => Decoder.decodeFloat
        case JDoubleT()  => Decoder.decodeDouble
        case JCharT()    => Decoder.decodeChar
        case JStrT()     => Decoder.decodeString
        case JArrayT(elem) =>
          Decoder.decodeList(sFromJ.decoder(elem)).map(_.toVector)
      }
    }

    val sFromJ: FromJson[Schema[JSchema, ?]] = FromJson.schemaFromJson(self)
  }
}
