package morphling

import cats.*

type HAlgebra[F[_[_], _], G[_]]   = F[G, _] ~> G
type HCoAlgebra[F[_[_], _], G[_]] = G ~> F[G, _]

type HCofree[F[_[_], _], A[_], I] = HFix[[Y[_], Z] =>> HEnvT[A, F, Y, Z], I]

trait HFunctor[F[_[_], _]] {
  def hlift[M[_], N[_]](nt: M ~> N): F[M, _] ~> F[N, _]

  extension [M[_], I](fa: F[M, I])(using HF: HFunctor[F]) def hfmap[N[_]](nt: M ~> N): F[N, I] = HF.hlift(nt)(fa)
}

object HFunctor {
  def apply[F[_[_], _]](using hf: HFunctor[F]): HFunctor[F] = hf
}

/**
 * Fixpoint data type that can preserve a type index through its recursive step.
 */
final case class HFix[F[_[_], _], I](unfix: Eval[F[HFix[F, _], I]])

object HFix {
  import HFunctor.*

  def hfix[F[_[_], _], I](fa: => F[HFix[F, _], I]): HFix[F, I] =
    HFix[F, I](Later(fa))

  def cataNT[F[_[_], _]: HFunctor, G[_]](alg: HAlgebra[F, G]): HFix[F, _] ~> G =
    new ((HFix[F, _]) ~> G) { self =>
      def apply[I](f: HFix[F, I]): G[I] =
        alg.apply[I](f.unfix.value.hfmap[G](self))
    }

  def anaNT[F[_[_], _]: HFunctor, G[_]](alg: HCoAlgebra[F, G]): G ~> HFix[F, _] =
    new (G ~> (HFix[F, _])) { self =>
      override def apply[I](fa: G[I]): HFix[F, I] =
        hfix(alg.apply[I](fa).hfmap(self))
    }

  /** Smart constructor for HCofree values. */
  def hcofree[F[_[_], _], A[_], I](ask: A[I], fga: => F[HCofree[F, A, _], I]): HCofree[F, A, I] =
    hfix[[Y[_], Z] =>> HEnvT[A, F, Y, Z], I](HEnvT(ask, fga))

  /**
   * Algebra to discard the annotations from an HCofree structure.
   */
  def forgetAlg[F[_[_], _], A[_]]: HEnvT[A, F, HFix[F, _], _] ~> HFix[F, _] =
    new HAlgebra[[Y[_], Z] =>> HEnvT[A, F, Y, Z], HFix[F, _]] {
      def apply[I](env: HEnvT[A, F, HFix[F, _], I]): HFix[F, I] = hfix(env.fa)
    }

  def forget[F[_[_], _]: HFunctor, A[_]]: HCofree[F, A, _] ~> HFix[F, _] = cataNT(forgetAlg)

  /**
   * Algebra to annotate the whole HCofree with a same annotation
   */
  def annotateAlg[F[_[_], _], A[_]](ann: A[Nothing]): HFix[F, _] ~> HEnvT[A, F, HFix[F, _], _] =
    new HCoAlgebra[[Y[_], Z] =>> HEnvT[A, F, Y, Z], HFix[F, _]] {
      override def apply[T](fa: HFix[F, T]): HEnvT[A, F, HFix[F, _], T] =
        HEnvT[A, F, HFix[F, _], T](ann.asInstanceOf[A[T]], fa.unfix.value)
    }

  def annotate[F[_[_], _]: HFunctor, A[_]](ann: A[Nothing]): HFix[F, _] ~> HCofree[F, A, _] = anaNT(annotateAlg(ann))
}

given [F[_[_], _]: HFunctor]: HFunctor[[Y[_], Z] =>> HCofree[F, Y, Z]] with {
  override def hlift[M[_], N[_]](nt: M ~> N): HCofree[F, M, _] ~> HCofree[F, N, _] =
    new (HCofree[F, M, *] ~> HCofree[F, N, *]) {
      override def apply[I](hc: HCofree[F, M, I]): HCofree[F, N, I] = {
        val step = hc.unfix.value
        HFix.hcofree(
          nt.apply(step.ask),
          HFunctor[F].hlift(HFunctor[[Y[_], Z] =>> HCofree[F, Y, Z]].hlift(nt)).apply(step.fa)
        )
      }
    }
}

final case class HMutu[F[_[_], _], G[_[_], _], I](unmutu: F[G[HMutu[F, G, _], _], I]) {
  type Inner[T] = G[HMutu[F, G, _], T]

  def transformInner[H[_[_], _]](f: Inner ~> H[HMutu[F, H, _], _])(using hfg: HFunctor[F]): HMutu[F, H, I] =
    HMutu(unmutu.hfmap(f))
}

final case class HEnvT[E[_], F[_[_], _], G[_], I](ask: E[I], fa: F[G, I])

given [E[_], F[_[_], _]: HFunctor]: HFunctor[[Y[_], Z] =>> HEnvT[E, F, Y, Z]] with {
  override def hlift[M[_], N[_]](nt: M ~> N): HEnvT[E, F, M, _] ~> HEnvT[E, F, N, _] =
    new (HEnvT[E, F, M, _] ~> HEnvT[E, F, N, _]) {
      override def apply[I](fm: HEnvT[E, F, M, I]): HEnvT[E, F, N, I] = HEnvT(fm.ask, fm.fa.hfmap[N](nt))
    }
}
