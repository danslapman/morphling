package morphling.tapir

import cats.*
import cats.data.{Const, EitherK}
import cats.free.FreeApplicative
import morphling.*
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import simulacrum_.typeclass
import sttp.tapir.SchemaType.{SCoproduct, SDiscriminator, SProduct, SProductField}
import sttp.tapir.{FieldName, Schema as TapirSchema, SchemaType, Validator}

@typeclass
trait ToSchema[S[_]] {
  def toSchema: S ~> TapirSchema
}

object ToSchema {
  implicit class ToSchemaOps[S[_], A](s: S[A]) {
    def schema(implicit TT: ToSchema[S]): TapirSchema[A] = TT.toSchema(s)
  }

  implicit def schemaToSchema[P[_]: ToSchema]: ToSchema[Schema[P, *]] = new ToSchema[Schema[P, *]] {
    override val toSchema: Schema[P, *] ~> TapirSchema = new (Schema[P, *] ~> TapirSchema) {
      override def apply[I](schema: Schema[P, I]): TapirSchema[I] =
        HFix.cataNT[SchemaF[P, *[_], *], TapirSchema](schemaAlg[P]).apply(schema)
    }
  }

  implicit def annSchemaToSchema[P[_]: ToSchema, A[_]: *[_] ~> λ[T => Endo[TapirSchema[T]]]]
      : ToSchema[AnnotatedSchema[P, A, *]] =
    new ToSchema[AnnotatedSchema[P, A, *]] {
      override val toSchema: AnnotatedSchema[P, A, *] ~> TapirSchema =
        new (AnnotatedSchema[P, A, *] ~> TapirSchema) {
          override def apply[I](schema: AnnotatedSchema[P, A, I]): TapirSchema[I] =
            HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], TapirSchema](annSchemaAlg).apply(schema)
        }
    }

  def schemaAlg[P[_]: ToSchema]: HAlgebra[SchemaF[P, *[_], *], TapirSchema] =
    new HAlgebra[SchemaF[P, *[_], *], TapirSchema] {
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
      interpret: Ann ~> λ[T => Endo[TapirSchema[T]]]
  ): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], TapirSchema] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], TapirSchema] {
      override def apply[A](schema: HEnvT[Ann, SchemaF[P, *[_], *], TapirSchema, A]): TapirSchema[A] =
        interpret.apply(schema.ask).apply(schemaAlg[P].apply(schema.fa))
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
                      (req.extract.getOption _).andThen(_.orElse(Option(dv)))
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

  implicit def eitherKTSchema[P[_]: ToSchema, Q[_]: ToSchema]: ToSchema[EitherK[P, Q, *]] =
    new ToSchema[EitherK[P, Q, *]] {
      override val toSchema: EitherK[P, Q, *] ~> TapirSchema =
        new (EitherK[P, Q, *] ~> TapirSchema) {
          override def apply[A](fa: EitherK[P, Q, A]): TapirSchema[A] = fa.run.fold(
            ToSchema[P].toSchema(_),
            ToSchema[Q].toSchema(_),
          )
        }
    }
}
