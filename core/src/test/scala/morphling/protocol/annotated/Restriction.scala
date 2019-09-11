package morphling.protocol.annotated

import alleycats.EmptyK

sealed trait Restriction[T]
case object Non extends Restriction[Nothing]
case class Range(from: Int, to: Int) extends Restriction[Int]
object Restriction {
  implicit val restrictionEmptyK: EmptyK[Restriction] = new EmptyK[Restriction] {
    override def empty[A]: Restriction[A] = Non.asInstanceOf[Restriction[A]]
  }
}