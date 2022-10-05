package morphling.protocol.annotated

import alleycats.EmptyK

sealed trait Restriction[T]
case class Non[A]() extends Restriction[A]
object Non {
  private val nonOfNothing = Non[Nothing]()

  def of[A]: Non[A] = nonOfNothing.asInstanceOf[Non[A]]
}

case class Range(from: Int, to: Int) extends Restriction[Int]
object Restriction {
  implicit val restrictionEmptyK: EmptyK[Restriction] = new EmptyK[Restriction] {
    override def empty[A]: Restriction[A] = Non.of[A]
  }
}
