package morphling.scalacheck

import cats.*
import cats.data.EitherK
import cats.free.*
import morphling.*
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import org.scalacheck.Gen
import simulacrum_.typeclass

@typeclass
trait ToGen[S[_]] {
  def toGen: S ~> Gen
}

object ToGen {
  implicit class ToGenOps[S[_], A](s: S[A]) {
    def gen(implicit TG: ToGen[S]): Gen[A] = TG.toGen(s)
  }

  implicit def schemaToGen[P[_]: ToGen]: ToGen[Schema[P, *]] = new ToGen[Schema[P, *]] {
    override val toGen: Schema[P, *] ~> Gen = new (Schema[P, *] ~> Gen) {
      override def apply[I](schema: Schema[P, I]): Gen[I] =
        HFix.cataNT[SchemaF[P, *[_], *], Gen](genAlg).apply(schema)
    }
  }

  implicit def annSchemaToGen[P[_]: ToGen, A[_]: *[_] ~> λ[T => Endo[Gen[T]]]]: ToGen[AnnotatedSchema[P, A, *]] =
    new ToGen[AnnotatedSchema[P, A, *]] {
      override val toGen: AnnotatedSchema[P, A, *] ~> Gen = new (AnnotatedSchema[P, A, *] ~> Gen) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): Gen[I] =
          HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], Gen](annGenAlg).apply(schema)
      }
    }

  def genAlg[P[_]: ToGen]: HAlgebra[SchemaF[P, *[_], *], Gen] =
    new HAlgebra[SchemaF[P, *[_], *], Gen] {
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

  def annGenAlg[P[_]: ToGen, Ann[_]](implicit
      interpret: Ann ~> λ[T => Endo[Gen[T]]]
  ): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], Gen] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], Gen] {
      override def apply[I](schema: HEnvT[Ann, SchemaF[P, *[_], *], Gen, I]): Gen[I] =
        interpret.apply(schema.ask).apply(genAlg[P].apply(schema.fa))
    }

  def recordGen[P[_]: ToGen, I](rb: FreeApplicative[PropSchema[I, Gen, *], I]): Gen[I] = {
    implicit val djap: Applicative[Gen] = new Applicative[Gen] {
      override def pure[T](x: T): Gen[T] = Gen.const(x)

      override def ap[T, U](ff: Gen[T => U])(fa: Gen[T]): Gen[U] =
        fa.flatMap(a => ff.map(_(a)))
    }

    rb.foldMap(
      new (PropSchema[I, Gen, *] ~> Gen) {
        def apply[B](ps: PropSchema[I, Gen, B]): Gen[B] = ps match {
          case Required(_, base, _, _)  => base
          case opt: Optional[I, Gen, i] => Gen.option(opt.base)
          case Constant(_, value, _)    => Gen.const(value)
          case abs: Absent[I, Gen, i]   => Gen.const(Option.empty[i])
        }
      }
    )
  }

  implicit def eitherKGen[P[_]: ToGen, Q[_]: ToGen]: ToGen[EitherK[P, Q, *]] = new ToGen[EitherK[P, Q, *]] {
    override val toGen: EitherK[P, Q, *] ~> Gen = new (EitherK[P, Q, *] ~> Gen) {
      override def apply[A](fa: EitherK[P, Q, A]): Gen[A] = fa.run.fold(
        ToGen[P].toGen(_),
        ToGen[Q].toGen(_),
      )
    }
  }
}
