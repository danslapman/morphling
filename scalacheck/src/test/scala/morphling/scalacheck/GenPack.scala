package morphling.scalacheck

import cats.~>
import morphling.protocol._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._

trait GenPack {
  def sTypeGen[F[_]: ToGen]: (SType[F, *] ~> Gen) =
    new (SType[F, *] ~> Gen) {
      import ToGen._

      override def apply[A](st: SType[F, A]): Gen[A] = st match {
        case SNullT()   => arbitrary[Unit]
        case SBoolT()   => arbitrary[Boolean]
        case SIntT()    => arbitrary[Int]
        case SLongT()   => Gen.chooseNum(Long.MinValue + 808L, Long.MaxValue) // Magic number to circumvent Instant#toEpochMillis throwing exceptions
        case SFloatT()  => arbitrary[Float]
        case SDoubleT() => arbitrary[Double]
        case SCharT()   => arbitrary[Char]
        case SStrT()    => arbitrary[String]
        case arr: SArrayT[F, i] =>
          containerOf[Vector, i](arr.elem.gen)
      }
    }
}
