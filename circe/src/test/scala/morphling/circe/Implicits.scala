package morphling.circe

import cats._
import io.circe.{Decoder, Json}
import morphling.protocol._
import morphling.protocol.JType.JSchema
import morphling.Schema.Schema

object Implicits {
  implicit val primToJson: ToJson[JSchema] = new ToJson[JSchema] { self =>
    val serialize = new (JSchema ~> (? => Json)) {
      def apply[I](s: JSchema[I]): I => Json = s.unmutu match {
        case JNullT()    => (_: I) => Json.Null
        case JBoolT()    => Json.fromBoolean(_)
        case JIntT()     => Json.fromInt(_)
        case JLongT()    => Json.fromLong(_)
        case JFloatT()   => Json.fromFloatOrString(_)
        case JDoubleT()  => Json.fromDoubleOrString(_)
        case JCharT()    => c => Json.fromString(c.toString)
        case JStrT()     => Json.fromString(_)
        case JArrayT(elem) =>
          xs => Json.fromValues(xs.map(sToJ.serialize(elem)).toList)
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
