package morphling.reactivemongo

import cats.~>
import morphling.protocol.SType.SSchema
import reactivemongo.api.bson._

object Implicits extends ReactivemongoPack {
  implicit val primToBson: ToBson[SSchema] = new ToBson[SSchema] { self =>
    val writer: SSchema ~> BSONWriter = new (SSchema ~> BSONWriter) {
      override def apply[I](s: SSchema[I]): BSONWriter[I] =
        sTypeWriter[SSchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromBson: FromBson[SSchema] = new FromBson[SSchema] { self =>
    val reader = new (SSchema ~> BSONReader) {
      def apply[I](s: SSchema[I]): BSONReader[I] =
        sTypeReader[SSchema[I]#Inner].apply(s.unmutu)
    }
  }
}
