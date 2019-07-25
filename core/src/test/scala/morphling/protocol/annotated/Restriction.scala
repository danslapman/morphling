package morphling.protocol.annotated

sealed trait Restriction
case object NoRestr extends Restriction
case class Range(from: BigDecimal, to: BigDecimal) extends Restriction

object Restriction {
  val Non: Restriction = NoRestr
}
