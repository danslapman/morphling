import cats.~>

package object morphling {
  type HAlgebra[F[_[_], _], G[_]] = F[G, *] ~> G
  type HCoAlgebra[F[_[_], _], G[_]] = G ~> F[G, *]

  type HCofree[F[_[_], _], A[_], I] = HFix[HEnvT[A, F, *[_], *], I]
}
