package morphling.scalacheck

import scala.annotation.implicitNotFound

import cats.*
import cats.data.EitherK
import cats.free.*
import morphling.*
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import morphling.given
import mouse.option.*
import org.scalacheck.Gen
import simulacrum.typeclass

@implicitNotFound("Could not find an instance of ToGen for ${S}")
@typeclass
trait ToGen[S[_]] extends Serializable {
  def toGen: S ~> Gen

  extension [S[_], A](s: S[A])(using TG: ToGen[S]) def gen: Gen[A] = TG.toGen(s)
}

object ToGen {
  given [P[_]: ToGen]: ToGen[Schema[P, _]] =
    new ToGen[Schema[P, _]] {
      override val toGen: Schema[P, _] ~> Gen = new (Schema[P, _] ~> Gen) {
        override def apply[I](schema: Schema[P, I]): Gen[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], Gen](genAlg).apply(schema)
      }
    }

  given [P[_]: ToGen, A[_]: [Y[_]] =>> Y ~> ([T] =>> Endo[Gen[T]])]: ToGen[AnnotatedSchema[P, A, _]] =
    new ToGen[AnnotatedSchema[P, A, *]] {
      override val toGen: AnnotatedSchema[P, A, _] ~> Gen = new (AnnotatedSchema[P, A, _] ~> Gen) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): Gen[I] =
          HFix.cataNT[[Y1[_], Z1] =>> HEnvT[A, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Gen](annGenAlg).apply(schema)
      }
    }

  def genAlg[P[_]: ToGen]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Gen] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Gen] {
      def apply[I](schema: SchemaF[P, Gen, I]): Gen[I] = schema match {
        case s: PrimSchema[P, Gen, I] => ToGen[P].toGen(s.prim)
        case s: OneOfSchema[P, Gen, I] =>
          val altGens = s.alts.map { case Alt(_, b, p) => b.map(p.upcast) }
          altGens.tail.headOption.cata(
            th => Gen.oneOf(altGens.head, th, altGens.tail.tail*),
            altGens.head
          )

        case s: RecordSchema[P, Gen, I]  => recordGen[P, I](s.props)
        case s: IsoSchema[P, Gen, i0, I] => s.base.map(s.eqv.get(_))
      }
    }

  def annGenAlg[P[_]: ToGen, Ann[_]](using
      interpret: Ann ~> ([T] =>> Endo[Gen[T]])
  ): HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Gen] =
    new HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Gen] {
      override def apply[I](schema: HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Gen, I]): Gen[I] =
        interpret.apply(schema.ask).apply(genAlg[P].apply(schema.fa))
    }

  def recordGen[P[_]: ToGen, I](rb: FreeApplicative[PropSchema[I, Gen, _], I]): Gen[I] = {
    implicit val djap: Applicative[Gen] = new Applicative[Gen] {
      override def pure[T](x: T): Gen[T] = Gen.const(x)

      override def ap[T, U](ff: Gen[T => U])(fa: Gen[T]): Gen[U] =
        fa.flatMap(a => ff.map(_(a)))
    }

    rb.foldMap(
      new (PropSchema[I, Gen, _] ~> Gen) {
        def apply[B](ps: PropSchema[I, Gen, B]): Gen[B] = ps match {
          case Required(_, base, _, _)             => base
          case opt: Optional[I, Gen, i] @unchecked => Gen.option(opt.base)
          case Constant(_, value, _)               => Gen.const(value)
          case abs: Absent[I, Gen, i] @unchecked   => Gen.const(Option.empty[i])
        }
      }
    )
  }

  given [P[_]: ToGen, Q[_]: ToGen]: ToGen[EitherK[P, Q, _]] =
    new ToGen[EitherK[P, Q, _]] {
      override val toGen: EitherK[P, Q, _] ~> Gen = new (EitherK[P, Q, _] ~> Gen) {
        override def apply[A](fa: EitherK[P, Q, A]): Gen[A] = fa.run.fold(
          ToGen[P].toGen(_),
          ToGen[Q].toGen(_),
        )
      }
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ToGen]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: ToGen[S]): ToGen[S] = instance

  object ops {
    implicit def toAllToGenOps[S[_], A](target: S[A])(implicit tc: ToGen[S]): AllOps[S, A] {
      type TypeClassType = ToGen[S]
    } = new AllOps[S, A] {
      type TypeClassType = ToGen[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: ToGen[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToToGenOps extends Serializable {
    implicit def toToGenOps[S[_], A](target: S[A])(implicit tc: ToGen[S]): Ops[S, A] {
      type TypeClassType = ToGen[S]
    } = new Ops[S, A] {
      type TypeClassType = ToGen[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToToGenOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
