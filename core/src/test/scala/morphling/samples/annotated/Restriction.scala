package morphling.samples.annotated

sealed trait Restriction
case object NoRestr extends Restriction
case class Range(from: BigDecimal, to: BigDecimal) extends Restriction
