package morphling.annotated

import alleycats.EmptyK
import cats.*
import cats.arrow.Profunctor
import cats.data.NonEmptyList
import cats.free.*
import morphling.*
import morphling.HFix.*
import glass.{Property as TProp, _}

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
  /** The type of an annotated schema.
    *
    * This is an alias for the HFix fixpoint applied to the SchemaF type constructor.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    */
  type AnnotatedSchema[P[_], A[_], I] = HCofree[[Y[_], Z] =>> SchemaF[P, Y, Z], A, I]

  /** The type of free applicative values which are used to capture the structure
    * of individual record properties.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam O $ODefn
    * @tparam I $IDefn
    */
  type Prop[P[_], A[_], O, I] = FreeApplicative[PropSchema[O, AnnotatedSchema[P, A, _], _], I]

  given [P[_], A[_], O]: Applicative[Prop[P, A, O, _]] =
    FreeApplicative.freeApplicative[PropSchema[O, AnnotatedSchema[P, A, _], _]]

  given[P[_], A[_]]: Profunctor[[Y, Z] =>> Prop[P, A, Y, Z]] =
    new Profunctor[[Y, Z] =>> Prop[P, A, Y, Z]] {
      override def dimap[O, I, N, J](prop: Prop[P, A, O, I])(f: N => O)(g: I => J): Prop[P, A, N, J] =
        prop.compile[PropSchema[N, AnnotatedSchema[P, A, _], _]](
          PropSchema.contraNT[O, N, AnnotatedSchema[P, A, _]](f)
        ).map(g)
    }

  /** The type of free applicative values which are used to capture the structure
    * of record (product) types.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam R The type of the Scala value to be produced (or consumed)
    *           by an interpreter of the schema. This is usually the type
    *           of a record - an object or a tuple.
    */
  type Props[P[_], A[_], R] = Prop[P, A, R, R]

  /** Lifts a SchemaF value into an annotated Schema
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param sf The value to be annotated
    * @return the newly constructed schema value
    */
  def schema[P[_], A[_], I](sf: => SchemaF[P, AnnotatedSchema[P, A, _], I], ann: => A[I]): AnnotatedSchema[P, A, I] =
    hcofree[[Y[_], Z] =>> SchemaF[P, Y, Z], A, I](ann, sf)

  /** Lifts a SchemaF value into an annotated Schema
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param sf The value to be annotated
    * @return the newly constructed schema value
    */
  def schema[P[_], A[_] : EmptyK, I](sf: => SchemaF[P, AnnotatedSchema[P, A, *], I]): AnnotatedSchema[P, A, I] =
    hcofree[[Y[_], Z] =>> SchemaF[P, Y, Z], A, I](EmptyK[A].empty, sf)

  /** Lifts a value in an algebra of primitives into an annotated Schema
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param p a value of the `P` algebra
    * @return the newly constructed schema value
    */
  def prim[P[_], A[_], I](p: P[I], ann: A[I]): AnnotatedSchema[P, A, I] =
    schema(PrimSchema[P, AnnotatedSchema[P, A, _], I](p), ann)

  /** Lifts a value in an algebra of primitives into an annotated Schema
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param p a value of the `P` algebra
    * @return the newly constructed schema value
    */
  def prim[P[_], A[_] : EmptyK, I](p: P[I]): AnnotatedSchema[P, A, I] =
    schema(PrimSchema[P, AnnotatedSchema[P, A, _], I](p), EmptyK[A].empty)

  /** Builds an annotated schema for a record type from the free
    * applicative capture of that record's structure.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param props The free-applicative value that captures the structure
    *              of the record type.
    */
  def rec[P[_], A[_], I](props: Props[P, A, I], ann: A[I]): AnnotatedSchema[P, A, I] =
    schema(RecordSchema[P, AnnotatedSchema[P, A, _], I](props), ann)

  /** Builds an annotated schema for a record type from the free
    * applicative capture of that record's structure.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @param props The free-applicative value that captures the structure
    *              of the record type.
    */
  def rec[P[_], A[_] : EmptyK, I](props: Props[P, A, I]): AnnotatedSchema[P, A, I] =
    schema(RecordSchema[P, AnnotatedSchema[P, A, _], I](props), EmptyK[A].empty)

  /** Smart constructor for required Prop instances.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam O $ODefn
    * @tparam I $IDefn
    * @param fieldName   name of the record property
    * @param valueSchema schema for the record property's type
    * @param extract     Extract lens from the record type to the property's value
    */
  def required[P[_], A[_], O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], extract: Extract[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], I](
      Required[O, AnnotatedSchema[P, A, _], I](fieldName, valueSchema, extract, None)
    )
  }

  /** Smart constructor for required Prop instances, with a default
    * provided for the case where a serialized form is missing the
    * required field.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam O $ODefn
    * @tparam I $IDefn
    * @param fieldName   Name of the record property
    * @param valueSchema Schema for the record property's type
    * @param default     Default value for use in the case that a serialized form
    *                    is missing the required field.
    * @param extract     Extract lens from the record type to the property's value
    */
  def property[P[_], A[_], O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], default: I, extract: Extract[O, I]): Prop[P, A, O, I] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], I](
      Required[O, AnnotatedSchema[P, A, _], I](fieldName, valueSchema, extract, Some(default))
    )
  }

  /** Smart constructor for optional Prop instances.
    *
    * @tparam P  $PDefn
    * @tparam A  $ADefn
    * @tparam O  $ODefn
    * @tparam I  $IDefn
    * @tparam OI hack for proper overload resolution
    * @param fieldName   name of the record property
    * @param valueSchema schema for the record property's type
    * @param extract     Extract lens from the record type to the property's value
    */
  def optional[P[_], A[_], O, I, OI <: Option[I]](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], extract: Extract[O, OI]): Prop[P, A, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], Option[I]](
      Optional[O, AnnotatedSchema[P, A, _], I](fieldName, valueSchema, extract.asInstanceOf[Extract[O, Option[I]]])
    )
  }

  /** Smart constructor for optional Prop instances.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam O $ODefn
    * @tparam I $IDefn
    * @param fieldName   name of the record property
    * @param valueSchema schema for the record property's type
    * @param property    Property lens from the record type to the property's value
    */
  def optional[P[_], A[_], O, I](fieldName: String, valueSchema: AnnotatedSchema[P, A, I], property: TProp[O, I]): Prop[P, A, O, Option[I]] = {
    FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], Option[I]](
      Optional[O, AnnotatedSchema[P, A, _], I](fieldName, valueSchema, property.getOption _)
    )
  }

  /**
    * Smart constructor for absent Prop instances.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    */
  def absent[P[_], A[_]]: AbsentBuilder[P, A] = new AbsentBuilder[P, A]

  /**
    * Builder class used to construct a Absent property
    */
  final class AbsentBuilder[P[_], A[_]] {
    def apply[O, I, OI <: Option[I]](fieldName: String, extract: Extract[O, OI]): Prop[P, A, O, Option[I]] = {
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], Option[I]](
        Absent[O, AnnotatedSchema[P, A, _], I](fieldName, extract.asInstanceOf[Extract[O, Option[I]]])
      )
    }

    /*
    def apply[O, I](fieldName: String, property: TProp[O, I]): Prop[P, A, O, Option[I]] = {
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], Option[I]](
        Absent[O, AnnotatedSchema[P, A, _], I](fieldName, property.getOption _)
      )
    }*/
  }

  /**
    * Smart constructor for constant Prop instances.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    */
  def constant[P[_], A[_]]: ConstantBuilder[P, A] = new ConstantBuilder[P, A]

  /**
    * Builder class used to construct a Constant property
    */
  final class ConstantBuilder[P[_], A[_]] {
    def apply[O, I](fieldName: String, value: I, extract: Extract[O, I]): Prop[P, A, O, I] =
      FreeApplicative.lift[PropSchema[O, AnnotatedSchema[P, A, _], _], I](
        Constant(fieldName, value, extract)
      )
  }

  /** The annotated empty record schema, representing a constant value.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    */
  def const[P[_], A[_], O](obj: O, ann: A[O]): AnnotatedSchema[P, A, O] =
    rec[P, A, O](FreeApplicative.pure[PropSchema[O, AnnotatedSchema[P, A, _], _], O](obj), ann)

  /** The annotated empty record schema, representing a constant value.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    */
  def const[P[_], A[_] : EmptyK, O](obj: O): AnnotatedSchema[P, A, O] =
    rec[P, A, O](FreeApplicative.pure[PropSchema[O, AnnotatedSchema[P, A, _], _], O](obj))

  /*

    Need to reimplement Constructors first

  /** Builds an annotated schema for the sum type `I` from an HList of alternatives.
    *
    *  Each alternative value in the list describes a single constructor of `I`.
    *  For example, to construct the schema for [[scala.util.Either]] one would provide
    *  two alternatives, one for the `Left` constructor and one for `Right`.
    *
    *  An easier-to-read type signature for this function is below:
    *
    *  {{{
    *  def oneOf[P[_], I](alts: NonEmptyList[Alt[Schema[P, *], I, _]]): Schema[P, I]
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
    def apply[A[_], H <: HList](ann: A[I])(ctrs: H)(implicit ev: Constructors[I, Alt[AnnotatedSchema[P, A, *], I, *], H]): AnnotatedSchema[P, A, I] = {
      schema(OneOfSchema[P, AnnotatedSchema[P, A, *], I](ev.toNel(ctrs)), ann)
    }

    def apply[A[_]: EmptyK, H <: HList](ctrs: H)(implicit ev: Constructors[I, Alt[AnnotatedSchema[P, A, *], I, *], H]): AnnotatedSchema[P, A, I] = {
      schema(OneOfSchema[P, AnnotatedSchema[P, A, *], I](ev.toNel(ctrs)))
    }
  }

  def oneOfDiscr[P[_], I](discriminatorField: String): ToOneOfWithDiscriminator[P, I] =
    new ToOneOfWithDiscriminator[P, I](discriminatorField)

  /** Builder class used to construct a OneOfSchema value with discriminator field
    *  from an HList of alternatives which are proven to provide handling for
    *  every constructor of the sum type `I`.
    */
  final class ToOneOfWithDiscriminator[P[_], I](discriminatorField: String) {
    def apply[A[_], H <: HList](ann: A[I])(ctrs: H)(implicit ev: Constructors[I, Alt[AnnotatedSchema[P, A, *], I, *], H]): AnnotatedSchema[P, A, I] = {
      schema(OneOfSchema[P, AnnotatedSchema[P, A, *], I](ev.toNel(ctrs), Some(discriminatorField)), ann)
    }

    def apply[A[_]: EmptyK, H <: HList](ctrs: H)(implicit ev: Constructors[I, Alt[AnnotatedSchema[P, A, *], I, *], H]): AnnotatedSchema[P, A, I] = {
      schema(OneOfSchema[P, AnnotatedSchema[P, A, *], I](ev.toNel(ctrs), Some(discriminatorField)))
    }
  }
   */

  /** Builds an annotated schema for the sum type `I` from a list of alternatives.
    *
    * Each alternative value in the list describes a single constructor of `I`.
    * For example, to construct the schema for [[scala.util.Either]] one would provide
    * two alternatives, one for the `Left` constructor and one for `Right`.
    *
    * This convenience constructor is unsafe in that the compiler will not prove that
    * handling is present for every constructor of your sum type; however, it may sometimes
    * be needed in the case that failures of the Scala compiler to correctly identify
    * all the constructors of a sum type make it otherwise impossible to build a schema
    * value.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    */
  def unsafeOneOf[P[_], A[_], I](ann: A[I])(alts: NonEmptyList[Alt[AnnotatedSchema[P, A, _], I, ?]]): AnnotatedSchema[P, A, I] =
    schema(OneOfSchema[P, AnnotatedSchema[P, A, _], I](alts), ann)

  def unsafeOneOf[P[_], A[_] : EmptyK, I](alts: NonEmptyList[Alt[AnnotatedSchema[P, A, _], I, ?]]): AnnotatedSchema[P, A, I] =
    schema(OneOfSchema[P, AnnotatedSchema[P, A, _], I](alts))

  def unsafeOneOfDiscr[P[_], A[_], I](discriminatorField: String, ann: A[I])(alts: NonEmptyList[Alt[AnnotatedSchema[P, A, _], I, ?]]): AnnotatedSchema[P, A, I] =
    schema(OneOfSchema[P, AnnotatedSchema[P, A, _], I](alts, Some(discriminatorField)), ann)

  def unsafeOneOfDiscr[P[_], A[_] : EmptyK, I](discriminatorField: String)(alts: NonEmptyList[Alt[AnnotatedSchema[P, A, _], I, ?]]): AnnotatedSchema[P, A, I] =
    schema(OneOfSchema[P, AnnotatedSchema[P, A, _], I](alts, Some(discriminatorField)))

  /** Convenience constructor for oneOf schema alternatives.
    *
    * @tparam P $PDefn
    * @tparam A $ADefn
    * @tparam I $IDefn
    * @tparam J The type of the base value which can be mapped into the `I` algebra.
    * @param id     The unique identifier of the constructor
    * @param base   The schema for the `J` type
    * @param subset Subset between the sum type and the selected constructor.
    */
  def alt[P[_], A[_], I, J](id: String, base: AnnotatedSchema[P, A, J], subset: Subset[I, J]): Alt[AnnotatedSchema[P, A, _], I, J] =
    Alt[AnnotatedSchema[P, A, _], I, J](id, base, subset)

  extension [P[_], A[_], I](base: AnnotatedSchema[P, A, I]) {
    def composeIso[J](eqv: Equivalent[I, J], transform: A[I] => A[J]): AnnotatedSchema[P, A, J] = {
      schema(IsoSchema[P, AnnotatedSchema[P, A, _], I, J](base, eqv), transform(base.unfix.value.ask))
    }
  }
}