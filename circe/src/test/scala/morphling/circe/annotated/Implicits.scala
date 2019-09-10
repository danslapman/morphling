package morphling.circe.annotated

import cats._
import io.circe.{AccumulatingDecoder, Decoder, Encoder}
import morphling.circe.{CircePack, FromJson, ToJson}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.protocol.annotated.{Non, Range, Restriction}

object Implicits extends CircePack {
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

  implicit val primToJson: ToJson[ASchema] = new ToJson[ASchema] {
    val encoder = new (ASchema ~> Encoder) {
      def apply[I](s: ASchema[I]): Encoder[I] = sTypeEncoder[ASchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromJson: FromJson[ASchema] = new FromJson[ASchema] {
    val decoder = new (ASchema ~> Decoder) {
      def apply[I](s: ASchema[I]): Decoder[I] = sTypeDecoder[ASchema[I]#Inner].apply(s.unmutu)
    }

    val accumulatingDecoder: ASchema ~> AccumulatingDecoder =
      decoder.andThen(λ[Decoder ~> AccumulatingDecoder](AccumulatingDecoder.fromDecoder(_)))
  }
}
