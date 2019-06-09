package morphling.annotated

import cats._
import cats.arrow.Profunctor
import morphling._
//import cats.data.NonEmptyList
import cats.free._
import monocle.{Optional => _, _}
import morphling.HFix._
//import morphling.HFunctor._

//import shapeless.{Prism => _, _}

object Schema {
  type Schema[P[_], A, I] = HCofree[SchemaF[P, ?[_], ?], A, I]

  type Prop[P[_], A, O, I] = FreeApplicative[PropSchema[O, Schema[P, A, ?], ?], I]

  implicit def propApplicative[P[_], A, O]: Applicative[Prop[P, A, O, ?]] =
    FreeApplicative.freeApplicative[PropSchema[O, Schema[P, A, ?], ?]]

  implicit def propProfunctor[P[_], A]: Profunctor[Prop[P, A, ?, ?]] = new Profunctor[Prop[P, A, ?, ?]] {
    override def dimap[O, I, N, J](prop: Prop[P, A, O, I])(f: N => O)(g: I => J): Prop[P, A, N, J] =
      prop.compile[PropSchema[N, Schema[P, A, ?], ?]](
        PropSchema.contraNT[O, N, Schema[P, A, ?]](f)
      ).map(g)
  }

  type Props[P[_], A, R] = Prop[P, A, R, R]

  def schema[P[_], A, I](sf: => SchemaF[P, Schema[P, A, ?], I], ann: => A): Schema[P, A, I] =
    hcofree[SchemaF[P, ?[_], ?], A, I](ann, sf)

  def prim[P[_], A, I](p: P[I], ann: A): Schema[P, A, I] =
    schema(PrimSchema[P, Schema[P, A, ?], I](p), ann)

  def rec[P[_], A, I](props: Props[P, A, I], ann: A): Schema[P, A, I] =
    schema(RecordSchema[P, Schema[P, A, ?], I](props), ann)

  def required[P[_], A, O, I](fieldName: String, valueSchema: Schema[P, A, I], getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, A, ?], ?], I](
      Required[O, Schema[P, A, ?], I](fieldName, valueSchema, getter, None)
    )
  }

  def property[P[_], A, O, I](fieldName: String, valueSchema: Schema[P, A, I], default: I, getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, A, ?], ?], I](
      Required[O, Schema[P, A, ?], I](fieldName, valueSchema, getter, Some(default))
    )
  }

  def optional[P[_], A, O, I](fieldName: String, valueSchema: Schema[P, A, I], getter: Getter[O, Option[I]]): Prop[P, A, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, A, ?], ?], Option[I]](
      Optional[O, Schema[P, A, ?], I](fieldName, valueSchema, getter)
    )
  }
}
