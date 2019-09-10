package morphling.protocol.annotated

sealed trait Restriction[T]
case object Non extends Restriction[Nothing]
case class Range(from: Int, to: Int) extends Restriction[Int]
object Restriction {
  def non[T]: Restriction[T] = Non.asInstanceOf[Restriction[T]]
}