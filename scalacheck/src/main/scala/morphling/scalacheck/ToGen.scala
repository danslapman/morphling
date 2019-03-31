package morphling.scalacheck

import cats._
import cats.free._
import morphling.HFunctor._
import morphling._
import morphling.Schema.Schema
import mouse.option._
import org.scalacheck.Gen
import simulacrum.typeclass

@typeclass
trait ToGen[S[_]] {
  def toGen: S ~> Gen
}

object ToGen {
  implicit class ToGenOps[S[_], A](s: S[A]) {
    def toGen(implicit TG: ToGen[S]): Gen[A] = TG.toGen(s)
  }

  implicit def schemaToGen[P[_]: ToGen]: ToGen[Schema[P, ?]] = new ToGen[Schema[P, ?]] {
    def toGen: Schema[P, ?] ~> Gen = new (Schema[P, ?] ~> Gen) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], Gen](genAlg).apply(schema)
      }
    }
  }

  def genAlg[P[_]: ToGen]: HAlgebra[SchemaF[P, ?[_], ?], Gen] =
    new HAlgebra[SchemaF[P, ?[_], ?], Gen] {
      def apply[I](schema: SchemaF[P, Gen, I]): Gen[I] = schema match {
        case s: PrimSchema[P, Gen, I] => ToGen[P].toGen(s.prim)
        case s: OneOfSchema[P, Gen, I] =>
          val altGens = s.alts.map({ case Alt(_, b, p) => b.map(p.reverseGet) })
          altGens.tail.headOption.cata(
            th => Gen.oneOf(altGens.head, th, altGens.tail.toList.tail: _*),
            altGens.head
          )

        case s: RecordSchema[P, Gen, I] => recordGen[P,I](s.props)
        case s: IsoSchema[P, Gen, i0, I] => s.base.map(s.iso.get(_))
      }
    }

  def recordGen[P[_]: ToGen, I](rb: FreeApplicative[PropSchema[I, Gen, ?], I]): Gen[I] = {
    implicit val djap: Applicative[Gen] = new Applicative[Gen] {
      override def pure[T](x: T): Gen[T] = Gen.const(x)

      override def ap[T, U](ff: Gen[T => U])(fa: Gen[T]): Gen[U] = {
        fa.flatMap(a => ff.map(_(a)))
      }
    }

    rb.foldMap(
      new (PropSchema[I, Gen, ?] ~> Gen) {
        def apply[B](ps: PropSchema[I, Gen, B]): Gen[B] = ps match {
          case Required(_, base, _, _) => base
          case opt: Optional[I, Gen, i] => Gen.option(opt.base)
        }
      }
    )
  }
}