package morphling.annotated

import cats._
import cats.arrow.Profunctor
import morphling._
import shapeless.HList
//import cats.data.NonEmptyList
import cats.free._
import monocle.{Optional => MOptional, _}
import morphling.HFix._
//import morphling.HFunctor._

//import shapeless.{Prism => _, _}

object Schema {
  type AnnotatedSchema[P[_], A, I] = HCofree[SchemaF[P, *[_], *], A, I]

  type Prop[P[_], A, O, I] = FreeApplicative[PropSchema[O, AnnotatedSchema[P, A, *], *], I]

  implicit def propApplicative[P[_], A, O]: Applicative[Prop[P, A, O, *]] =
    FreeApplicative.freeApplicative[PropSchema[O, AnnotatedSchema[P, A, *], *]]

  implicit def propProfunctor[P[_], A]: Profunctor[Prop[P, A, *, *]] = new Profunctor[Prop[P, A, *, *]] {
    override def dimap[O, I, N, J](prop: Prop[P, A, O, I])(f: N => O)(g: I => J): Prop[P, A, N, J] =
      prop.compile[PropSchema[N, AnnotatedSchema[P, A, *], *]](
        PropSchema.contraNT[O, N, AnnotatedSchema[P, A, *]](f)
      ).map(g)
  }

  type Props[P[_], A, R] = Prop[P, A, R, R]

  def schema[P[_], A, I](sf: => SchemaF[P, AnnotatedSchema[P, A, *], I], ann: => A): AnnotatedSchema[P, A, I] =
    hcofree[SchemaF[P, *[_], *], A, I](ann, sf)

  def prim[P[_], A, I](p: P[I], ann: A): AnnotatedSchema[P, A, I] =
    schema(PrimSchema[P, AnnotatedSchema[P, A, *], I](p), ann)

  def rec[P[_], A, I](props: Props[P, A, I], ann: A): AnnotatedSchema[P, A, I] =
    schema(RecordSchema[P, AnnotatedSchema[P, A, *], I](props), ann)

  def required[P[_], A, O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
      Required[O, AnnotatedSchema[P, A, *], I](fieldName, valueSchema, getter, None)
    )
  }

  def required[P[_], A, O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], lens: Lens[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
      Required[O, AnnotatedSchema[P, A, *], I](fieldName, valueSchema, lens.asGetter, None)
    )
  }

  def property[P[_], A, O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], default: I, getter: Getter[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
      Required[O, AnnotatedSchema[P, A, *], I](fieldName, valueSchema, getter, Some(default))
    )
  }

  def property[P[_], A, O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], default: I, lens: Lens[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
      Required[O, AnnotatedSchema[P, A, *], I](fieldName, valueSchema, lens.asGetter, Some(default))
    )
  }

  def optional[P[_], A, O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], getter: Getter[O, Option[I]]): Prop[P, A, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], Option[I]](
      Optional[O, AnnotatedSchema[P, A, *], I](fieldName, valueSchema, getter)
    )
  }

  def absent[P[_], A]: AbsentBuilder[P, A] = new AbsentBuilder[P, A]

  final class AbsentBuilder[P[_], A] {
    def apply[O, I](fieldName: String, getter: Getter[O, Option[I]]): Prop[P, A, O, Option[I]] = {
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], Option[I]](
        Absent[O, AnnotatedSchema[P, A, *], I](fieldName, getter)
      )
    }

    def apply[O, I](fieldName: String, lens: Lens[O, Option[I]]): Prop[P, A, O, Option[I]] = {
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], Option[I]](
        Absent[O, AnnotatedSchema[P, A, *], I](fieldName, lens.asGetter)
      )
    }

    def apply[O, I](fieldName: String, optional: MOptional[O, I]): Prop[P, A, O, Option[I]] = {
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], Option[I]](
        Absent[O, AnnotatedSchema[P, A, *], I](fieldName, Getter(optional.getOption))
      )
    }
  }

  def constant[P[_], A]: ConstantBuilder[P, A] = new ConstantBuilder[P, A]

  final class ConstantBuilder[P[_], A] {
    def apply[O, I](fieldName: String, value: I, getter: Getter[O, I]): Prop[P, A, O, I] =
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
        Constant(fieldName, value, getter)
      )

    def apply[O, I](fieldName: String, value: I, lens: Lens[O, I]): Prop[P, A, O, I] =
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, *], *], I](
        Constant(fieldName, value, lens.asGetter)
      )
  }

  def const[P[_], A, O](obj: O, ann: A): AnnotatedSchema[P, A, O] =
    rec[P, A, O](FreeApplicative.pure[PropSchema[O, AnnotatedSchema[P, A, *], *], O](obj), ann)

  def oneOf[P[_], I]: ToOneOf[P, I] = new ToOneOf[P, I]

  final class ToOneOf[P[_], I] {
    def apply[A, H <: HList](ann: A)(ctrs: H)(implicit ev: Constructors[I, Alt[AnnotatedSchema[P, A, *], I, *], H]): AnnotatedSchema[P, A, I] = {
      schema(OneOfSchema[P, AnnotatedSchema[P, A, *], I](ev.toNel(ctrs)), ann)
    }
  }

  def alt[P[_], A, I, J](id: String, base: AnnotatedSchema[P, A, J], prism: Prism[I, J]): Alt[AnnotatedSchema[P, A, *], I, J] =
    Alt[AnnotatedSchema[P, A, *], I, J](id, base, prism)

  implicit class SchemaOps[P[_], A, I](base: AnnotatedSchema[P, A, I]) {
    def composeIso[J](iso: Iso[I, J]): AnnotatedSchema[P, A, J] = {
      schema(IsoSchema[P, AnnotatedSchema[P, A, *], I, J](base, iso), base.unfix.value.ask)
    }
  }
}
