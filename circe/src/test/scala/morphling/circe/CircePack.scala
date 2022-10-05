package morphling.circe

import cats.data.Const
import cats.~>
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import morphling.protocol.*

trait CircePack {
  def sTypeEncoder[F[_]: ToJson]: SType[F, *] ~> Encoder =
    new (SType[F, *] ~> Encoder) {
      import ToJson.*

      override def apply[A](st: SType[F, A]): Encoder[A] = st match {
        case SNullT()          => Encoder.encodeUnit
        case SBoolT()          => Encoder.encodeBoolean
        case SIntT()           => Encoder.encodeInt
        case SLongT()          => Encoder.encodeLong
        case SFloatT()         => Encoder.encodeFloat
        case SDoubleT()        => Encoder.encodeDouble
        case SCharT()          => Encoder.encodeChar
        case SStrT()           => Encoder.encodeString
        case sa: SArrayT[F, i] => Encoder.encodeVector(sa.elem.encoder)
      }
    }

  def sTypeDecoder[F[_]: FromJson]: SType[F, *] ~> Decoder =
    new (SType[F, *] ~> Decoder) {
      import FromJson.*

      override def apply[A](st: SType[F, A]): Decoder[A] = st match {
        case SNullT()          => Decoder.decodeUnit
        case SBoolT()          => Decoder.decodeBoolean
        case SIntT()           => Decoder.decodeInt
        case SLongT()          => Decoder.decodeLong
        case SFloatT()         => Decoder.decodeFloat
        case SDoubleT()        => Decoder.decodeDouble
        case SCharT()          => Decoder.decodeChar
        case SStrT()           => Decoder.decodeString
        case sa: SArrayT[F, i] => Decoder.decodeVector(sa.elem.decoder)
      }
    }

  def sTypeFilter[F[_]: ToFilter]: SType[F, *] ~> Const[Json => Option[Json], *] =
    new (SType[F, *] ~> Const[Json => Option[Json], *]) {
      import ToFilter.*

      override def apply[A](st: SType[F, A]): Const[Json => Option[Json], A] = Const.of(st match {
        case SNullT()          => _.asNull.map(_ => Json.Null)
        case SBoolT()          => _.asBoolean.map(Json.fromBoolean)
        case SIntT()           => _.asNumber.map(Json.fromJsonNumber)
        case SLongT()          => _.asNumber.map(Json.fromJsonNumber)
        case SFloatT()         => _.asNumber.map(Json.fromJsonNumber)
        case SDoubleT()        => _.asNumber.map(Json.fromJsonNumber)
        case SCharT()          => _.asString.map(Json.fromString)
        case SStrT()           => _.asString.map(Json.fromString)
        case sa: SArrayT[F, i] => _.asArray.map(_.flatMap(sa.elem.jsonFilter.apply)).map(Json.fromValues)
      })
    }
}
