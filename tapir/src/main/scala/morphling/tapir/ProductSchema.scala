package morphling.tapir

import sttp.tapir.Schema
import sttp.tapir.SchemaType.SProduct

object ProductSchema {
  def unapply[T](schema: Schema[T]): Option[SProduct[T]] =
    schema match {
      case Schema(sp @ SProduct(_), _, _, _, _, _, _, _, _, _, _) => Some(sp)
      case _                                                      => None
    }
}
