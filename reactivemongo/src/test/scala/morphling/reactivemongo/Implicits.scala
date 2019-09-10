package morphling.reactivemongo

import cats.~>
import morphling.protocol.SType.SSchema
import reactivemongo.bson._

object Implicits extends ReactivemongoPack {
  implicit val primToBson: ToBson[SSchema] = new ToBson[SSchema] { self =>
    val writer: SSchema ~> BSONWriter[*, BSONValue] = new (SSchema ~> BSONWriter[*, BSONValue]) {
      override def apply[I](s: SSchema[I]): BSONWriter[I, BSONValue] =
        sTypeWriter[SSchema[I]#Inner].apply(s.unmutu)
    }
  }

  implicit val primFromBson: FromBson[SSchema] = new FromBson[SSchema] { self =>
    val reader = new (SSchema ~> BSONReader[BSONValue, *]) {
      def apply[I](s: SSchema[I]): BSONReader[BSONValue, I] =
        sTypeReader[SSchema[I]#Inner].apply(s.unmutu)
    }
  }
}
