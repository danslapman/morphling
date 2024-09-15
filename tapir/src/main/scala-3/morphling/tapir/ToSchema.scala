package morphling.tapir

import cats.*
import cats.data.{Const, EitherK}
import cats.free.*
import morphling.*
import morphling.Schema.*
import morphling.annotated.Schema.AnnotatedSchema
import morphling.given
import mouse.option.*
import simulacrum.typeclass
import sttp.tapir.SchemaType.{SCoproduct, SDiscriminator, SProduct, SProductField}
import sttp.tapir.{FieldName, Schema as TapirSchema, SchemaType, Validator}

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of ToSchema for ${S}")
@typeclass
trait ToSchema[S[_]] extends Serializable {
  def toSchema: S ~> TapirSchema

  extension [F[_], A](fa: F[A])(using TS: ToSchema[F]) def schema: TapirSchema[A] = TS.toSchema(fa)
}

object ToSchema {
  given [P[_]: ToSchema]: ToSchema[Schema[P, _]] =
    new ToSchema[Schema[P, _]] {
      override val toSchema: Schema[P, _] ~> TapirSchema = new (Schema[P, _] ~> TapirSchema) {
        override def apply[I](schema: Schema[P, I]): TapirSchema[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], TapirSchema](schemaAlg).apply(schema)
      }
    }

  given [P[_]: ToSchema, A[_]: [Y[_]] =>> Y ~> ([T] =>> Endo[TapirSchema[T]])]: ToSchema[AnnotatedSchema[P, A, *]] =
    new ToSchema[AnnotatedSchema[P, A, _]] {
      override val toSchema: AnnotatedSchema[P, A, _] ~> TapirSchema = new (AnnotatedSchema[P, A, _] ~> TapirSchema) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): TapirSchema[I] =
          HFix
            .cataNT[[Y1[_], Z1] =>> HEnvT[A, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], TapirSchema](annSchemaAlg[P, A])
            .apply(schema)
      }
    }

  def schemaAlg[P[_]: ToSchema]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], TapirSchema] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], TapirSchema] {
      def apply[I](schema: SchemaF[P, TapirSchema, I]): TapirSchema[I] = schema match {
        case s: PrimSchema[P, TapirSchema, I] => ToSchema[P].toSchema(s.prim)

        case s: OneOfSchema[P, TapirSchema, I] =>
          s.discriminator.cata(
            dField => {
              val discriminator = SDiscriminator(FieldName(dField, dField), Map.empty)

              TapirSchema(
                SCoproduct(
                  s.alts.map { case Alt(id, schema, subs) =>
                    val discriminatorField = SProductField[I, String](
                      FieldName(dField),
                      TapirSchema.schemaForString,
                      (t: I) => subs.getOption(t).map(_ => id)
                    )

                    schema match {
                      case ProductSchema(product) =>
                        schema.copy(
                          schemaType = product
                            .copy(fields = (discriminatorField :: product.fields).asInstanceOf[List[SProductField[Any]]])
                            .asInstanceOf[SchemaType[Any]],
                          validator = schema.validator.asInstanceOf[Validator[Any]]
                        )
                      case _ => schema
                    }
                  }.toList,
                  Some(discriminator)
                )(_ => None)
              )
            },
            TapirSchema(
              SCoproduct(
                s.alts.map { case Alt(id, schema, subs) =>
                  TapirSchema(SProduct(SProductField(FieldName(id), schema, subs.getOption) :: Nil))
                }.toList,
                None
              )(_ => None)
            )
          )

        case s: RecordSchema[P, TapirSchema, I]  => recordSchema[P, I](s.props)
        case s: IsoSchema[P, TapirSchema, i0, I] => s.base.as[I]
      }
    }

  def annSchemaAlg[P[_]: ToSchema, Ann[_]](implicit
      interpret: Ann ~> ([T] =>> Endo[TapirSchema[T]])
  ): HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], TapirSchema] =
    new HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], TapirSchema] {
      override def apply[I](s: HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], TapirSchema, I]): TapirSchema[I] =
        interpret(s.ask).apply(schemaAlg[P].apply(s.fa))
    }

  def recordSchema[P[_]: ToSchema, I](rb: FreeApplicative[PropSchema[I, TapirSchema, *], I]): TapirSchema[I] = {
    val fields = rb
      .foldMap[Const[List[SProductField[I]], *]](
        new (PropSchema[I, TapirSchema, *] ~> Const[List[SProductField[I]], *]) {
          override def apply[A](ps: PropSchema[I, TapirSchema, A]): Const[List[SProductField[I]], A] =
            ps match {
              case req: Required[I, TapirSchema, i] =>
                req.default.cata(
                  (dv: i) => {
                    val optionalField = SProductField(
                      FieldName(req.fieldName),
                      req.base.default(dv, None),
                      req.extract.getOption.andThen(_.orElse(Option(dv)))
                    )
                    Const.of(optionalField :: Nil)
                  }, {
                    val requiredField = SProductField(
                      FieldName(req.fieldName),
                      req.base,
                      req.extract.getOption
                    )
                    Const.of(requiredField :: Nil)
                  }
                )
              case opt: Optional[I, TapirSchema, i] =>
                val optionalField = SProductField(
                  FieldName(opt.fieldName),
                  opt.base.copy(isOptional = true),
                  opt.extract.extract
                )
                Const.of(optionalField :: Nil)
              case const: Constant[I, TapirSchema, i] =>
                Const.of(Nil)
              case abs: Absent[I, TapirSchema, i] =>
                Const.of(Nil)
            }
        }
      )
      .getConst

    TapirSchema(SProduct(fields))
  }

  given [P[_]: ToSchema, Q[_]: ToSchema]: ToSchema[EitherK[P, Q, _]] =
    new ToSchema[EitherK[P, Q, _]] {
      override val toSchema = new (EitherK[P, Q, _] ~> TapirSchema) {
        def apply[A](p: EitherK[P, Q, A]): TapirSchema[A] =
          p.run.fold(ToSchema[P].toSchema(_), ToSchema[Q].toSchema(_))
      }
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ToSchema]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: ToSchema[S]): ToSchema[S] = instance

  object ops {
    implicit def toAllToSchemaOps[S[_], A](target: S[A])(implicit tc: ToSchema[S]): AllOps[S, A] {
      type TypeClassType = ToSchema[S]
    } = new AllOps[S, A] {
      type TypeClassType = ToSchema[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: ToSchema[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToToSchemaOps extends Serializable {
    implicit def toToSchemaOps[S[_], A](target: S[A])(implicit tc: ToSchema[S]): Ops[S, A] {
      type TypeClassType = ToSchema[S]
    } = new Ops[S, A] {
      type TypeClassType = ToSchema[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToToSchemaOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
