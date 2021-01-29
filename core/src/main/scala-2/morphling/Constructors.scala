package morphling

import scala.annotation.implicitNotFound

import cats.data.NonEmptyList
import cats.syntax.list._
import shapeless.{Prism => _, _}
import shapeless.ops.coproduct.ToHList
import shapeless.ops.hlist.{Align, Comapped, ToTraversable}

/** Implicit proof type
  *
  */
@implicitNotFound(msg = "Cannot prove the completeness of your oneOf definition; you may have not provided an alternative for each constructor of your sum type ${I}")
sealed trait Constructors[I, F[_], H <: HList] {
  def toNel(h: H): NonEmptyList[F[_]]
}

object Constructors {
  implicit def evidence[I, F[_], C <: Coproduct, H0 <: HList, H1 <: HList, H <: HList](implicit
     G: Generic.Aux[I, C],
     L: ToHList.Aux[C, H1],
     M: Comapped.Aux[H, F, H0],
     A: Align[H0, H1],
     T: ToTraversable.Aux[H, List, F[_]]): Constructors[I, F, H] = new Constructors[I, F, H] {
    def toNel(h: H): NonEmptyList[F[_]] = {
      h.toList.toNel.get
    }
  }
}