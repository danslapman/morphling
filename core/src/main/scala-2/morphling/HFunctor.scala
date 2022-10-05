package morphling

import cats.*

trait HFunctor[F[_[_], _]] {
  def hlift[M[_], N[_]](nt: M ~> N): F[M, *] ~> F[N, *]
}

object HFunctor {
  def apply[F[_[_], _]](implicit v: HFunctor[F]): HFunctor[F] = v

  implicit final class HFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: HFunctor[F]) {
    def hfmap[N[_]](nt: M ~> N): F[N, A] = F.hlift(nt)(fa)
  }
}

/**
 * Fixpoint data type that can preserve a type index through its recursive step.
 */
final case class HFix[F[_[_], _], I](unfix: Eval[F[HFix[F, *], I]])

object HFix {
  import HFunctor.*

  def hfix[F[_[_], _], I](fa: => F[HFix[F, *], I]): HFix[F, I] =
    HFix[F, I](Later(fa))

  def cataNT[F[_[_], _]: HFunctor, G[_]](alg: HAlgebra[F, G]): HFix[F, *] ~> G =
    new (HFix[F, *] ~> G) { self =>
      def apply[I](f: HFix[F, I]): G[I] =
        alg.apply[I](f.unfix.value.hfmap[G](self))
    }

  def anaNT[F[_[_], _]: HFunctor, G[_]](alg: HCoAlgebra[F, G]): G ~> HFix[F, *] =
    new (G ~> HFix[F, *]) { self =>
      override def apply[I](fa: G[I]): HFix[F, I] =
        hfix(alg.apply[I](fa).hfmap(self))
    }

  /** Smart constructor for HCofree values. */
  def hcofree[F[_[_], _], A[_], I](ask: A[I], fga: => F[HCofree[F, A, *], I]): HCofree[F, A, I] =
    hfix[HEnvT[A, F, *[_], *], I](HEnvT(ask, fga))

  /**
   * Algebra to discard the annotations from an HCofree structure.
   */
  def forgetAlg[F[_[_], _], A[_]]: HEnvT[A, F, HFix[F, *], *] ~> HFix[F, *] =
    new HAlgebra[HEnvT[A, F, *[_], *], HFix[F, *]] {
      def apply[I](env: HEnvT[A, F, HFix[F, *], I]): HFix[F, I] = hfix(env.fa)
    }

  def forget[F[_[_], _]: HFunctor, A[_]]: HCofree[F, A, *] ~> HFix[F, *] = cataNT(forgetAlg)

  /**
   * Algebra to annotate the whole HCofree with a same annotation
   */
  def annotateAlg[F[_[_], _], A[_]](ann: A[Nothing]): HFix[F, *] ~> HEnvT[A, F, HFix[F, *], *] =
    new HCoAlgebra[HEnvT[A, F, *[_], *], HFix[F, *]] {
      override def apply[T](fa: HFix[F, T]): HEnvT[A, F, HFix[F, *], T] =
        HEnvT[A, F, HFix[F, *], T](ann.asInstanceOf[A[T]], fa.unfix.value)
    }

  def annotate[F[_[_], _]: HFunctor, A[_]](ann: A[Nothing]): HFix[F, *] ~> HCofree[F, A, *] = anaNT(annotateAlg(ann))

  /** HFunctor over the annotation type of an HCofree value */
  implicit def hCoFreeHFunctor[F[_[_], _]](implicit HF: HFunctor[F]): HFunctor[HCofree[F, *[_], *]] =
    new HFunctor[HCofree[F, *[_], *]] {
      override def hlift[M[_], N[_]](nt: M ~> N): HCofree[F, M, *] ~> HCofree[F, N, *] =
        new (HCofree[F, M, *] ~> HCofree[F, N, *]) {
          override def apply[I](hc: HCofree[F, M, I]): HCofree[F, N, I] = {
            val step = hc.unfix.value
            hcofree(nt.apply(step.ask), HF.hlift(hCoFreeHFunctor[F].hlift(nt)).apply(step.fa))
          }
        }
    }
}

//final case class HMutu[F[_[_], _], G[_[_], _], I](unmutu: F[G[HMutu[F, G, *], *], I]) {
final case class HMutu[F[_[_], _], G[_[_], _], I](unmutu: F[HMutu.Inner[F, G]#IAux, I]) {
  // type Inner[T] = G[HMutu[F, G, *], T]
  type Inner[T] = G[HMutu.Aux[F, G]#Aux, T]

  def transformInner[H[_[_], _]](f: Inner ~> H[HMutu[F, H, *], *])(implicit hfg: HFunctor[F]): HMutu[F, H, I] =
    HMutu(hfg.hlift(f)(unmutu))
}

object HMutu {
  type Aux[F[_[_], _], G[_[_], _]] = {
    type Aux[I] = HMutu[F, G, I]
  }

  type Inner[F[_[_], _], G[_[_], _]] = {
    type IAux[I] = G[Aux[F, G]#Aux, I]
  }
}

final case class HEnvT[E[_], F[_[_], _], G[_], I](ask: E[I], fa: F[G, I])

object HEnvT {
  import HFunctor.*

  implicit def hEnvTHFunctor[E[_], F[_[_], _]: HFunctor]: HFunctor[HEnvT[E, F, *[_], *]] =
    new HFunctor[HEnvT[E, F, *[_], *]] {
      def hlift[M[_], N[_]](nt: M ~> N): HEnvT[E, F, M, *] ~> HEnvT[E, F, N, *] =
        new (HEnvT[E, F, M, *] ~> HEnvT[E, F, N, *]) {
          def apply[I](fm: HEnvT[E, F, M, I]): HEnvT[E, F, N, I] = HEnvT(fm.ask, fm.fa.hfmap[N](nt))
        }
    }
}
