package morphling

import cats._
import cats.arrow.Profunctor
//import cats.data.NonEmptyList
import cats.free._
import monocle.{Optional => MOptional, _}
import morphling.HFix._
//import morphling.HFunctor._

//import shapeless.{Prism => _, _}

object AnnSchema {
  type AnnSchema[P[_], A, I] = HCofree[SchemaF[P, ?[_], ?], A, I]

  type Prop[P[_], A, O, I] = FreeApplicative[PropSchema[O, AnnSchema[P, A, ?], ?], I]

  implicit def propApplicative[P[_], A, O]: Applicative[Prop[P, A, O, ?]] =
    FreeApplicative.freeApplicative[PropSchema[O, AnnSchema[P, A, ?], ?]]

  implicit def propProfunctor[P[_], A]: Profunctor[Prop[P, A, ?, ?]] = new Profunctor[Prop[P, A, ?, ?]] {
    override def dimap[O, I, N, J](prop: Prop[P, A, O, I])(f: N => O)(g: I => J): Prop[P, A, N, J] =
      prop.compile[PropSchema[N, AnnSchema[P, A, ?], ?]](
        PropSchema.contraNT[O, N, AnnSchema[P, A, ?]](f)
      ).map(g)
  }

  type Props[P[_], A, R] = Prop[P, A, R, R]

  def schema[P[_], A, I](sf: => SchemaF[P, AnnSchema[P, A, ?], I], ann: => A): AnnSchema[P, A, I] =
    hcofree[SchemaF[P, ?[_], ?], A, I](ann, sf)

  def prim[P[_], A, I](p: P[I], ann: A): AnnSchema[P, A, I] =
    schema(PrimSchema[P, AnnSchema[P, A, ?], I](p), ann)

  def rec[P[_], A, I](props: Props[P, A, I], ann: A): AnnSchema[P, A, I] =
    schema(RecordSchema[P, AnnSchema[P, A, ?], I](props), ann)

  def required[P[_], A, O, I](fieldName: String, valueSchema: AnnSchema[P, A, I], getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnSchema[P, A, ?], ?], I](
      Required[O, AnnSchema[P, A, ?], I](fieldName, valueSchema, getter, None)
    )
  }

  def property[P[_], A, O, I](fieldName: String, valueSchema: AnnSchema[P, A, I], default: I, getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnSchema[P, A, ?], ?], I](
      Required[O, AnnSchema[P, A, ?], I](fieldName, valueSchema, getter, Some(default))
    )
  }

  def optional[P[_], A, O, I](fieldName: String, valueSchema: AnnSchema[P, A, I], getter: Getter[O, Option[I]]): Prop[P, A, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, AnnSchema[P, A, ?], ?], Option[I]](
      Optional[O, AnnSchema[P, A, ?], I](fieldName, valueSchema, getter)
    )
  }
}
