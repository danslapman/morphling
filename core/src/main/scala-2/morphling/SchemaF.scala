package morphling

import cats._
import cats.data.NonEmptyList
import cats.free._
import tofu.optics._

/** The base trait for the schema GADT.
  *
  *  @define PDefn The GADT type constructor for a sum type which defines
  *          the set of primitive types used in the schema.
  *  @define IDefn The type of the Scala value to be produced (or consumed)
  *          by an interpreter of the schema. Also known as the "index" type
  *          of the schema.
  *  @define FDefn The functor through which the structure of the schema will
  *          be interpreted. This will almost always be a fixpoint type such as
  *          [[morphling.HFix.HCofree]], which is used to introduce the ability to
  *          create recursive (tree-structured) schema.
  *
  *  @tparam P $PDefn
  *  @tparam F $FDefn
  *  @tparam I $IDefn
  */
sealed trait SchemaF[P[_], F[_], I] {
  /** HFunctor operation which allows transformation of the
    *  functor through which the structure of the schema will
    *  be interpreted.
    *
    *  Defining this operation directly on the SchemaF type
    *  rather than in [[morphling.SchemaF.schemaFHFunctor]] simplifies
    *  type inference.
    */
  def hfmap[G[_]](nt: F ~> G): SchemaF[P, G, I]

  /** HFunctor operation which allows transformation of the
    *  primitive algebra of the schema.
    *
    *  Defining this operation directly on the SchemaF type
    *  rather than in [[morphling.SchemaF.schemaFHFunctor]] simplifies
    *  type inference.
    */
  def pmap[Q[_]](nt: P ~> Q): SchemaF[Q, F, I]
}

object SchemaF {
  implicit def schemaFHFunctor[P[_]]: HFunctor[SchemaF[P, *[_], *]] = new HFunctor[SchemaF[P, *[_], *]] {
    def hlift[M[_], N[_]](nt: M ~> N): SchemaF[P, M, *] ~> SchemaF[P, N, *] =
      new (SchemaF[P, M, *] ~> SchemaF[P, N, *]) {
        def apply[I](fa: SchemaF[P, M, I]): SchemaF[P, N, I] = fa.hfmap(nt)
      }
  }
}

/** Schema constructor that wraps a value of an underlying GADT
  *  of allowed primitive types.
  *
  *  The underlying GADT defines a set of types via GADT constructors;
  *  see [[morphling.protocol.SType]] for an example. This set of types
  *  defines what types may be treated as primitive (and have parsing/
  *  serialization/etc deferred to an external handler) when interpreting
  *  a schema value. For example, one might want to construct a GADT for
  *  for the Scala primitive types as such:
  *
  *  {{{
  *  sealed trait SType[I]
  *
  *  case object SNullT   extends SType[Unit]
  *  case object SBoolT   extends SType[Boolean]
  *
  *  case object SByteT   extends SType[Byte]
  *  case object SShortT  extends SType[Short]
  *  case object SIntT    extends SType[Int]
  *  case object SLongT   extends SType[Long]
  *
  *  case object SFloatT  extends SType[Float]
  *  case object SDoubleT extends SType[Double]
  *
  *  case object SCharT   extends SType[Char]
  *  case object SStrT    extends SType[String]
  *  }}}
  *
  *  This example treats String values as primitive as well, even though
  *  strictly speaking they're reference types, just because virtually
  *  any interpreter for a schema algebra will not want to represent
  *  strings in terms of sum or product types. The same might hold true
  *  for, for example, [[scala.Array]] but for the purposes of this example
  *  issues related to `ClassManifest` instances would introduce excessive
  *  complexity.
  *
  *  @tparam P $PDefn
  *  @tparam F $FDefn
  *  @tparam I $IDefn
  *  @param prim value identifying a primitive type.
  */
final case class PrimSchema[P[_], F[_], I](prim: P[I]) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G): PrimSchema[P, G, I] = PrimSchema[P, G, I](prim)
  def pmap[Q[_]](nt: P ~> Q): PrimSchema[Q, F, I] = PrimSchema[Q, F, I](nt(prim))
}

/** Constructor that enables creation of schema for sum types.
  *
  *  Each constructor of the sum type `I` is represented as a member
  *  of the list of alternatives. Each alternative defines a prism
  *  between a single constructor of the sum type, and an underlying
  *  type describing the arguments demanded by that constructor.
  *
  *  Consider the following sum type. The first constructor takes
  *  no arguments; the second takes two.
  *
  *  {{{
  *  sealed trait Role
  *
  *  case object User extends Role
  *  case class Administrator(department: String, subordinateCount: Int) extends Role
  *  }}}
  *
  *  A schema value for this type looks like:
  *
  *  {{{
  *  val roleSchema = oneOf(
  *    alt[Unit, Prim, Role, Unit](
  *      "user",
  *      Schema.empty,
  *      (_: Unit) => User,
  *      {
  *        case User => Some(Unit)
  *        case _ => None
  *      }
  *    ) ::
  *    alt[Unit, Prim, Role, Administrator](
  *      "administrator",
  *      rec[Prim, Administrator](
  *        (
  *          required("department", Prim.str, (_: Administrator).department),
  *          required("subordinateCount", Prim.int, (_: Administrator).subordinateCount)
  *        ).mapN(Administrator.apply)
  *      ),
  *      identity,
  *      {
  *        case a @ Administrator(_, _) => Some(a)
  *        case _ => None
  *      }
  *    ) :: Nil
  *  )
  *  }}}
  *
  *  @tparam P $PDefn
  *  @tparam F $FDefn
  *  @tparam I $IDefn
  */
final case class OneOfSchema[P[_], F[_], I](alts: NonEmptyList[Alt[F, I, _]], discriminator: Option[String] = None) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G): OneOfSchema[P, G, I] = OneOfSchema[P, G, I](alts.map(_.hfmap(nt)), discriminator)
  def pmap[Q[_]](nt: P ~> Q): OneOfSchema[Q, F, I] = OneOfSchema[Q, F, I](alts, discriminator)
}

/** A prism between a base type containing the arguments required by
  *  a single constructor of a sum type, and that sum type, along with
  *  the schema for the base type is used to describe those constructor
  *  arguments. The identifier is used to distinguish which constructor
  *  is being represented in the serialized form.
  *
  *  @define IDefn The type of the Scala value to be produced (or consumed)
  *          by an interpreter of the schema. Also known as the "index" type
  *          of the schema.
  *
  *  @define FDefn The functor through which the structure of the schema will
  *          be interpreted. This will almost always be a fixpoint type such as
  *          [[morphling.HFix.HCofree]], which is used to introduce the ability to
  *          create recursive (tree-structured) schema.
  *
  *  @tparam F $FDefn
  *  @tparam I $IDefn
  *  @tparam I0 The base type which corresponds to the arguments to
  *          the selected constructor.
  *  @param id The unique identifier of the constructor
  *  @param base The schema for the `I0` type
  *  @param subset Subset between the sum type and the selected constructor.
  */
final case class Alt[F[_], I, I0](id: String, base: F[I0], subset: Subset[I, I0]) {
  def hfmap[G[_]](nt: F ~> G): Alt[G, I, I0] = Alt(id, nt(base), subset)
}

/** Wrapper for the free applicative structure which is used to construct
  *  and disassemble values of product types.
  *
  *  @tparam P $PDefn
  *  @tparam F $FDefn
  *  @tparam I $IDefn
  *  @param props the free applicative value composed of zero or more PropSchema instances
  */
final case class RecordSchema[P[_], F[_], I](props: FreeApplicative[PropSchema[I, F, *], I]) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G): RecordSchema[P, G, I] = RecordSchema[P, G, I](props.compile[PropSchema[I, G, *]](PropSchema.propSchemaHFunctor[I].hlift[F, G](nt)))
  def pmap[Q[_]](nt: P ~> Q): RecordSchema[Q, F, I] = RecordSchema[Q, F, I](props)
}

/** Base trait for values which describe record properties.
  *
  *  @define FDefn The functor through which the structure of the schema will
  *          be interpreted. This will almost always be a fixpoint type such as
  *          [[morphling.HFix.HCofree]], which is used to introduce the ability to
  *          create recursive (tree-structured) schema.
  *
  *  @tparam O The record type.
  *  @tparam F $FDefn
  *  @tparam I The type of the property value.
  */
sealed trait PropSchema[O, F[_], I] {
  def fieldName: String
  def extract: Extract[O, I]

  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, I]
}

/** Class describing a required property of a record.
  *
  * @param fieldName The name of the property.
  * @param base Schema for the property's value type.
  * @param extract Extract lens from the record type to the property.
  * @param default Optional default value, for use in the case that a
  *        serialized form is missing the property.
  */
final case class Required[O, F[_], I](
  fieldName: String,
  base: F[I],
  extract: Extract[O, I],
  default: Option[I]
) extends PropSchema[O, F, I] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, I] =
    Required(fieldName, nt(base), extract, default)
}

/** Class describing an optional property of a record. Since in many
  *  serialized forms optional properties may be omitted entirely from
  *  the serialized form, a distinct type is needed in order to be able
  *  to correctly interpret the absence of a field.
  *
  * @param fieldName The name of the property.
  * @param base Schema for the property's value type.
  * @param extract Extract lens from the record type to the property.
  */
final case class Optional[O, F[_], I](
  fieldName: String,
  base: F[I],
  extract: Extract[O, Option[I]]
) extends PropSchema[O, F, Option[I]] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[I]] =
    Optional(fieldName, nt(base), extract)
}

/** Class describing an optional property of a record that is always absent.
  *
  * @param fieldName The name of the property.
  * @param extract Extract lens from the record type to the property.
  */
final case class Absent[O, F[_], I](
  fieldName: String,
  extract: Extract[O, Option[I]]
) extends PropSchema[O, F, Option[I]] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[I]] =
    Absent(fieldName, extract)
}

/**
  * Class describing a constant (non-serializable) property of a record.
  * @param fieldName The name of the property.
  * @param value The value of the property.
  * @param extract Extract lens from the record type to the property.
  */
final case class Constant[O, F[_], I](
  fieldName: String,
  value: I,
  extract: Extract[O, I]
) extends PropSchema[O, F, I] {
  override def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, I] =
    this.asInstanceOf[PropSchema[O, G, I]]
}

object PropSchema {
  implicit def propSchemaHFunctor[O]: HFunctor[PropSchema[O, *[_], *]] =
    new HFunctor[PropSchema[O, *[_], *]] {
      def hlift[M[_], N[_]](nt: M ~> N): PropSchema[O, M, *] ~> PropSchema[O, N, *] =
        new (PropSchema[O, M, *] ~> PropSchema[O, N, *]) {
          def apply[I](ps: PropSchema[O, M, I]): PropSchema[O, N, I] = ps.hfmap(nt)
        }
    }

  private def extract[A, B](f: A => B): Extract[A, B] = (s: A) => f(s)

  def contraNT[O, N, F[_]](f: N => O): PropSchema[O, F, *] ~> PropSchema[N, F, *] =
    new (PropSchema[O, F, *] ~> PropSchema[N, F, *]) {
      def apply[I](pso: PropSchema[O, F, I]): PropSchema[N, F, I] = {
        pso match {
          case Required(n, s, g, d) => Required(n, s, extract(f) >> g, d)
          case opt: Optional[O, F, i] => Optional(opt.fieldName, opt.base, extract(f) >> opt.extract)
          case Constant(fn, v, g) => Constant(fn, v, extract(f) >> g)
          case abs: Absent[O, F, i] => Absent(abs.fieldName, extract(f) >> abs.extract)
        }
      }
  }
}

case class IsoSchema[P[_], F[_], I, J](base: F[I], eqv: Equivalent[I, J]) extends SchemaF[P, F, J] {
  def hfmap[G[_]](nt: F ~> G): IsoSchema[P, G, I, J] = IsoSchema(nt(base), eqv)
  def pmap[Q[_]](nt: P ~> Q): IsoSchema[Q, F, I, J] = IsoSchema(base, eqv)
}