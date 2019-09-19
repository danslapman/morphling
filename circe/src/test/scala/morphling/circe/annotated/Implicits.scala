package morphling.circe.annotated

import cats._
import cats.data.{Const, Kleisli}
import cats.instances.option._
import io.circe.{AccumulatingDecoder, Decoder, Encoder, Json}
import morphling.circe.{CircePack, FromJson, ToFilter, ToJson}
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

  implicit val primToFilter: ToFilter[ASchema] = new ToFilter[ASchema] {
    val filter = new (ASchema ~> ToFilter.JsonFilter) {
      override def apply[I](s: ASchema[I]): ToFilter.JsonFilter[I] = sTypeFilter[ASchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val filterRestriction: (Restriction ~> λ[T => Endo[ToFilter.JsonFilter[T]]]) =
    new (Restriction ~> λ[T => Endo[ToFilter.JsonFilter[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[ToFilter.JsonFilter[A]] = rs match {
        case Non => identity
        case Range(from, to) =>
          (jf: ToFilter.JsonFilter[Int]) => Const.of[Int](
            Kleisli(jf.getConst).andThen((json: Json) => json.asNumber.filter(jn => jn.toInt.exists(n => n > from && n < to)).map(Json.fromJsonNumber)).run
          )
      }
    }
}
