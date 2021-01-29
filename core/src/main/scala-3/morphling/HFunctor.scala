package morphling

import cats._

type HAlgebra[F[_[_], _], G[_]] = ([Z] =>> F[G, Z]) ~> G
type HCoAlgebra[F[_[_], _], G[_]] = G ~> ([Z] =>> F[G, Z])

type HCofree[F[_[_], _], A[_], I] = HFix[[Y[_], Z] =>> HEnvT[A, F, Y, Z], I]

trait HFunctor[F[_[_], _]] {
  def hlift[M[_], N[_]](nt: M ~> N): ([Z] =>> F[M, Z]) ~> ([Z] =>> F[N, Z]) =
    new (([Z] =>> F[M, Z]) ~> ([Z] =>> F[N, Z])) {
      def apply[I](fm: F[M, I]): F[N, I] = fm.hfmap(nt)
    }
  extension [M[_], I](fa: F[M, I])
    def hfmap[N[_]](nt: M ~> N): F[N, I]
}

object HFunctor {
  def apply[F[_[_], _]](using hf: HFunctor[F]): HFunctor[F] = hf
}

/** Fixpoint data type that can preserve a type index through
  *  its recursive step.
  */
final case class HFix[F[_[_], _], I](unfix: Eval[F[[Z] =>> HFix[F, Z], I]])

object HFix {
  import HFunctor._

  def hfix[F[_[_], _], I](fa: => F[[Z] =>> HFix[F, Z], I]): HFix[F, I] =
    HFix[F, I](Later(fa))

  def cataNT[F[_[_], _]: HFunctor, G[_]](alg: HAlgebra[F, G]): ([Z] =>> HFix[F, Z]) ~> G =
    new (([Z] =>> HFix[F, Z]) ~> G) { self =>
      def apply[I](f: HFix[F, I]): G[I] = {
        alg.apply[I](f.unfix.value.hfmap[G](self))
      }
    }

  def anaNT[F[_[_], _]: HFunctor, G[_]](alg: HCoAlgebra[F, G]): G ~> ([Z] =>> HFix[F, Z]) =
    new (G ~> ([Z] =>> HFix[F, Z])) { self =>
      override def apply[I](fa: G[I]): HFix[F, I] = {
        hfix(alg.apply[I](fa).hfmap(self))
      }
    }

  /** Smart constructor for HCofree values. */
  def hcofree[F[_[_], _], A[_], I](ask: A[I], fga: => F[[Z] =>> HCofree[F, A, Z], I]): HCofree[F, A, I] =
    hfix[[Y[_], Z] =>> HEnvT[A, F, Y, Z], I](HEnvT(ask, fga))

  /**
    * Algebra to discard the annotations from an HCofree structure.
    */
  def forgetAlg[F[_[_], _], A[_]]: ([Z] =>> HEnvT[A, F, [Z1] =>> HFix[F, Z1], Z]) ~> ([Z] =>> HFix[F, Z]) =
    new HAlgebra[[Y[_], Z] =>> HEnvT[A, F, Y, Z], [Z] =>> HFix[F, Z]] {
      def apply[I](env: HEnvT[A, F, [Z] =>> HFix[F, Z], I]): HFix[F, I] = hfix(env.fa)
    }

  def forget[F[_[_], _]: HFunctor, A[_]]: ([Z] =>> HCofree[F, A, Z]) ~> ([Z] =>> HFix[F, Z]) = cataNT(forgetAlg)

  /**
    * Algebra to annotate the whole HCofree with a same annotation
    */
  def annotateAlg[F[_[_], _], A[_]](ann: A[Nothing]): ([Z] =>> HFix[F, Z]) ~> ([Z] =>> HEnvT[A, F, [Z1] =>> HFix[F, Z1], Z]) =
    new HCoAlgebra[[Y[_], Z] =>> HEnvT[A, F, Y, Z], [Z] =>> HFix[F, Z]] {
      override def apply[T](fa: HFix[F, T]): HEnvT[A, F, [Z] =>> HFix[F, Z], T] =
        HEnvT[A, F, [Z] =>> HFix[F, Z], T](ann.asInstanceOf[A[T]], fa.unfix.value)
    }

  def annotate[F[_[_], _]: HFunctor, A[_]](ann: A[Nothing]): ([Z] =>> HFix[F, Z]) ~> ([Z] =>> HCofree[F, A, Z]) = anaNT(annotateAlg(ann))
}

given [F[_[_], _]: HFunctor]: HFunctor[[Y[_], Z] =>> HCofree[F, Y, Z]] with {
  extension [M[_], I](hc: HCofree[F, M, I])
    def hfmap[N[_]](nt: M ~> N): HCofree[F, N, I] = {
      val step = hc.unfix.value
      HFix.hcofree(nt.apply(step.ask), step.fa.hfmap(HFunctor[[Y[_], Z] =>> HCofree[F, Y, Z]].hlift(nt)))
    }
}

final case class HMutu[F[_[_], _], G[_[_], _], I](unmutu: F[[Z] =>> G[[Z1] =>> HMutu[F, G, Z1], Z], I]) {
  type Inner[T] = G[[Z] =>> HMutu[F, G, Z], T]

  def transformInner[H[_[_], _]](f: Inner ~> ([Z] =>> H[[Z1] =>> HMutu[F, H, Z1], Z]))(using hfg: HFunctor[F]): HMutu[F, H, I] =
    HMutu(unmutu.hfmap(f))
}

final case class HEnvT[E[_], F[_[_], _], G[_], I](ask: E[I], fa: F[G, I])

given [E[_], F[_[_], _]: HFunctor]: HFunctor[[Y[_], Z] =>> HEnvT[E, F, Y, Z]] with {
  extension [M[_], I](fm: HEnvT[E, F, M, I])
    def hfmap[N[_]](nt: M ~> N): HEnvT[E, F, N, I] =
      HEnvT(fm.ask, fm.fa.hfmap[N](nt))
}