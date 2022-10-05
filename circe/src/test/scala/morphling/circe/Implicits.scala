package morphling.circe

import cats.*
import io.circe.Decoder
import io.circe.Encoder
import morphling.circe.ToFilter.JsonFilter
import morphling.protocol.SType.SSchema

object Implicits extends CircePack {
  implicit val primToJson: ToJson[SSchema] = new ToJson[SSchema] {
    val encoder = new (SSchema ~> Encoder) {
      def apply[I](s: SSchema[I]): Encoder[I] = sTypeEncoder[SSchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromJson: FromJson[SSchema] = new FromJson[SSchema] {
    val decoder = new (SSchema ~> Decoder) {
      def apply[I](s: SSchema[I]): Decoder[I] = sTypeDecoder[SSchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primToFilter: ToFilter[SSchema] = new ToFilter[SSchema] {
    val filter = new (SSchema ~> ToFilter.JsonFilter) {
      override def apply[I](s: SSchema[I]): JsonFilter[I] = sTypeFilter[SSchema[I]#Inner].apply(s.unmutu)
    }
  }
}
