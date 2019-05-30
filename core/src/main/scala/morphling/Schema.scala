package morphling

import cats._
import cats.arrow.Profunctor
import cats.data.NonEmptyList
import cats.free._
import monocle.{Optional => MOptional, _}
import morphling.HFix._
import morphling.HFunctor._

import shapeless.{Prism => _, Lens => _, _}

/** Data types and smart constructors which simplify the creation
  *  of schema values.
  *
  *  @define PDefn The GADT type constructor for a sum type which defines
  *          the set of primitive types used in the schema.
  *  @define IDefn The type of the Scala value to be produced (or consumed)
  *          by an interpreter of the schema. Also known as the "index" type
  *          of the schema.
  *  @define ODefn The type of a Scala record - an object or a tuple,
  *          the property of which is being defined.
  *  @define ADefn The type of the annotation applied to each node of the schema
  */
object Schema {
  /** The type of an unannotated schema.
    *
    *  This is an alias for the HFix fixpoint applied to the SchemaF type constructor.
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    */
  type Schema[P[_], I] = HFix[SchemaF[P, ?[_], ?], I]

  /** The type of free applicative values which are used to capture the structure
    *  of individual record properties.
    *
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    */
  type Prop[P[_], O, I] = FreeApplicative[PropSchema[O, Schema[P, ?], ?], I]

  implicit def propApplicative[P[_], O]: Applicative[Prop[P, O, ?]] =
    FreeApplicative.freeApplicative[PropSchema[O, Schema[P, ?], ?]]

  implicit def propProfunctor[P[_]]: Profunctor[Prop[P, ?, ?]] = new Profunctor[Prop[P, ?, ?]] {
    override def dimap[O, I, N, J](prop: Prop[P, O, I])(f: N => O)(g: I => J): Prop[P, N, J] =
      prop.compile[PropSchema[N, Schema[P, ?], ?]](
        PropSchema.contraNT[O, N, Schema[P, ?]](f)
      ).map(g)
  }

  /** The type of free applicative values which are used to capture the structure
    *  of record (product) types.
    *
    *  @tparam P $PDefn
    *  @tparam R The type of the Scala value to be produced (or consumed)
    *          by an interpreter of the schema. This is usually the type
    *          of a record - an object or a tuple.
    */
  type Props[P[_], R] = Prop[P, R, R]

  /** Lifts a SchemaF value into an unannotated Schema
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    *  @param sf The value to be annotated
    *  @return the newly constructed schema value
    */
  def schema[P[_], I](sf: => SchemaF[P, Schema[P, ?], I]): Schema[P, I] =
    hfix[SchemaF[P, ?[_], ?], I](sf)

  /** Lifts a value in an algebra of primitives into an unannotated Schema
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    *  @param p a value of the `P` algebra
    *  @return the newly constructed schema value
    */
  def prim[P[_], I](p: P[I]): Schema[P, I] =
    schema(PrimSchema[P, Schema[P, ?], I](p))

  /** Builds an un-annotated schema for a record type from the free
    *  applicative capture of that record's structure.
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    *  @param props The free-applicative value that captures the structure
    *         of the record type.
    */
  def rec[P[_], I](props: Props[P, I]): Schema[P, I] =
    schema(RecordSchema[P, Schema[P, ?], I](props))

  /** Smart constructor for required Prop instances.
    *
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName name of the record property
    *  @param valueSchema schema for the record property's type
    *  @param getter Getter lens from the record type to the property's value
    */
  def required[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], getter: Getter[O, I]): Prop[P, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, getter, None)
    )
  }

  /** Smart constructor for required Prop instances.
    *
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName name of the record property
    *  @param valueSchema schema for the record property's type
    *  @param lens Lens from the record type to the property's value
    */
  def required[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], lens: Lens[O, I]): Prop[P, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, lens.asGetter, None)
    )
  }

  /** Smart constructor for required Prop instances, with a default
    *  provided for the case where a serialized form is missing the
    *  required field.
    *
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName Name of the record property
    *  @param valueSchema Schema for the record property's type
    *  @param default Default value for use in the case that a serialized form
    *         is missing the required field.
    *  @param getter Getter lens from the record type to the property's value
    */
  def property[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], default: I, getter: Getter[O, I]): Prop[P, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, getter, Some(default))
    )
  }

  /** Smart constructor for required Prop instances, with a default
    *  provided for the case where a serialized form is missing the
    *  required field.
    *
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName Name of the record property
    *  @param valueSchema Schema for the record property's type
    *  @param default Default value for use in the case that a serialized form
    *         is missing the required field.
    *  @param lens Lens from the record type to the property's value
    */
  def property[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], default: I, lens: Lens[O, I]): Prop[P, O, I] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, lens.asGetter, Some(default))
    )
  }

  /** Smart constructor for optional Prop instances.
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName name of the record property
    *  @param valueSchema schema for the record property's type
    *  @param getter Getter lens from the record type to the property's value
    */
  def optional[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], getter: Getter[O, Option[I]]): Prop[P, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], Option[I]](
      Optional[O, Schema[P, ?], I](fieldName, valueSchema, getter)
    )
  }

  /** Smart constructor for optional Prop instances.
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName name of the record property
    *  @param valueSchema schema for the record property's type
    *  @param lens Lens from the record type to the property's value
    */
  def optional[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], lens: Lens[O, Option[I]]): Prop[P, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], Option[I]](
      Optional[O, Schema[P, ?], I](fieldName, valueSchema, lens.asGetter)
    )
  }

  /** Smart constructor for optional Prop instances.
    *  @tparam P $PDefn
    *  @tparam O $ODefn
    *  @tparam I $IDefn
    *  @param fieldName name of the record property
    *  @param valueSchema schema for the record property's type
    *  @param optional Optional lens from the record type to the property's value
    */
  def optional[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], optional: MOptional[O, I]): Prop[P, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], Option[I]](
      Optional[O, Schema[P, ?], I](fieldName, valueSchema, Getter(optional.getOption))
    )
  }

  /**
    * Smart constructor for constant Prop instances.
    *  @tparam P $PDefn
    */
  def constant[P[_]]: ConstantBuilder[P] = new ConstantBuilder[P]

  /**
    * Builder class used to construct a Constant property
    */
  final class ConstantBuilder[P[_]] {
    def apply[O, I](fieldName: String, value: I, getter: Getter[O, I]): Prop[P, O, I] =
      FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
        Constant(fieldName, value, getter)
      )

    def apply[O, I](fieldName: String, value: I, lens: Lens[O, I]): Prop[P, O, I] =
      FreeApplicative.lift[PropSchema[O, Schema[P, ?], ?], I](
        Constant(fieldName, value, lens.asGetter)
      )
  }

  /** The unannotated empty record schema, representing a constant value.
    *
    *  @tparam P $PDefn
    */
  def const[P[_], A](a: A): Schema[P, A] =
    rec[P, A](FreeApplicative.pure[PropSchema[A, Schema[P, ?], ?], A](a))

  /** Builds an un-annotated schema for the sum type `I` from an HList of alternatives.
    *
    *  Each alternative value in the list describes a single constructor of `I`.
    *  For example, to construct the schema for [[scala.util.Either]] one would provide
    *  two alternatives, one for the `Left` constructor and one for `Right`.
    *
    *  An easier-to-read type signature for this function is below:
    *
    *  {{{
    *  def oneOf[P[_], I](alts: NonEmptyList[Alt[Schema[P, ?], I, _]]): Schema[P, I]
    *  }}}
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    */
  def oneOf[P[_], I]: ToOneOf[P, I] = new ToOneOf[P, I]

  /** Builder class used to construct a OneOfSchema value from
    *  an HList of alternatives which are proven to provide handling for
    *  every constructor of the sum type `I`.
    */
  final class ToOneOf[P[_], I] {
    def apply[H <: HList](ctrs: H)(implicit ev: Constructors[I, Alt[Schema[P, ?], I, ?], H]): Schema[P, I] = {
      schema(OneOfSchema[P, Schema[P, ?], I](ev.toNel(ctrs)))
    }
  }

  def oneOfDiscr[P[_], I](discriminatorField: String): ToOneOfWithDiscriminator[P, I] =
    new ToOneOfWithDiscriminator[P, I](discriminatorField)

  /** Builder class used to construct a OneOfSchema value with discriminator field
    *  from an HList of alternatives which are proven to provide handling for
    *  every constructor of the sum type `I`.
    */
  final class ToOneOfWithDiscriminator[P[_], I](discriminatorField: String) {
    def apply[H <: HList](ctrs: H)(implicit ev: Constructors[I, Alt[Schema[P, ?], I, ?], H]): Schema[P, I] = {
      schema(OneOfSchema[P, Schema[P, ?], I](ev.toNel(ctrs), Some(discriminatorField)))
    }
  }

  /** Builds an un-annotated schema for the sum type `I` from a list of alternatives.
    *
    *  Each alternative value in the list describes a single constructor of `I`.
    *  For example, to construct the schema for [[scala.util.Either]] one would provide
    *  two alternatives, one for the `Left` constructor and one for `Right`.
    *
    *  This convenience constructor is unsafe in that the compiler will not prove that
    *  handling is present for every constructor of your sum type; however, it may sometimes
    *  be needed in the case that failures of the Scala compiler to correctly identify
    *  all the constructors of a sum type make it otherwise impossible to build a schema
    *  value.
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    */
  def unsafeOneOf[P[_], I](alts: NonEmptyList[Alt[Schema[P, ?], I, _]]): Schema[P, I] =
    schema(OneOfSchema[P, Schema[P, ?], I](alts))

  /** Convenience constructor for oneOf schema alternatives.
    *
    *  @tparam P $PDefn
    *  @tparam I $IDefn
    *  @tparam J The type of the base value which can be mapped into the `I` algebra.
    *  @param id The unique identifier of the constructor
    *  @param base The schema for the `J` type
    *  @param prism Prism between the sum type and the selected constructor.
    */
  def alt[P[_], I, J](id: String, base: Schema[P, J], prism: Prism[I, J]): Alt[Schema[P, ?], I, J] =
    Alt[Schema[P, ?], I, J](id, base, prism)

  /** HAlgebra for primitive type constructor transformation.
    */
  def hfmapAlg[P[_], Q[_]](nt: P ~> Q): SchemaF[P, Schema[Q, ?], ?] ~> Schema[Q, ?] =
    new HAlgebra[SchemaF[P, ?[_], ?], Schema[Q, ?]] {
      def apply[I](s: SchemaF[P, Schema[Q, ?], I]): Schema[Q, I] = hfix(s.pmap(nt))
    }

  /** Constructs the HFunctor instance for a Schema.
    *
    *  An easier-to-read type signature for this function is below:
    *
    *  {{{
    *  implicit def hfunctor: HFunctor[Schema]
    *  }}}
    */
  implicit def hfunctor: HFunctor[Schema] = new HFunctor[Schema] {
    def hfmap[P[_], Q[_]](nt: P ~> Q): Schema[P, ?] ~> Schema[Q, ?] = cataNT(hfmapAlg(nt))
  }

  implicit class SchemaOps[P[_], I](base: Schema[P, I]) {
    def composeIso[J](iso: Iso[I, J]): Schema[P, J] = {
      schema(IsoSchema[P, Schema[P, ?], I, J](base, iso))
    }
  }
}