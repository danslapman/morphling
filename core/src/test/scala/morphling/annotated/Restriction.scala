package morphling.annotated

sealed trait Restriction
case object NoRestr extends Restriction
case class Range[T: Numeric](from: T, to: T) extends Restriction
