package morphling.circe

import cats._
import cats.data.EitherK
import cats.free._
import cats.syntax.either._
import io.circe.{Decoder, DecodingFailure, HCursor}
import morphling._
import morphling.HFunctor._
import morphling.Schema._
import morphling.annotated.Schema.AnnotatedSchema
import mouse.boolean._
import simulacrum.typeclass

@typeclass
trait FromJson[S[_]] {
  def decoder: S ~> Decoder
}

object FromJson {
  implicit class FromJsonOps[F[_], A](fa: F[A]) {
    def decoder(implicit FJ: FromJson[F]): Decoder[A] = FJ.decoder(fa)
  }

  implicit def schemaFromJson[P[_]: FromJson]: FromJson[Schema[P, *]] = new FromJson[Schema[P, *]] {
    override val decoder: Schema[P, *] ~> Decoder = new (Schema[P, *] ~> Decoder) {
      override def apply[I](schema: Schema[P, I]): Decoder[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], Decoder](decoderAlg[P]).apply(schema)
      }
    }
  }

  implicit def annSchemaFromJson[P[_]: FromJson, A[_]: *[_] ~> λ[T => Endo[Decoder[T]]]]: FromJson[AnnotatedSchema[P, A, *]] = new FromJson[AnnotatedSchema[P, A, *]] {
    override val decoder: AnnotatedSchema[P, A, *] ~> Decoder = new (AnnotatedSchema[P, A, *] ~> Decoder) {
      override def apply[I](schema: AnnotatedSchema[P, A, I]): Decoder[I] = {
        HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], Decoder](annDecoderAlg[P, A]).apply(schema)
      }
    }
  }

  def decoderAlg[P[_]: FromJson]: HAlgebra[SchemaF[P, *[_], *], Decoder] =
    new HAlgebra[SchemaF[P, *[_], *], Decoder] {
      def apply[I](s: SchemaF[P, Decoder, I]): Decoder[I] = s match {
        case PrimSchema(p) => FromJson[P].decoder(p)

        case OneOfSchema(alts, None) =>
          Decoder.instance { c: HCursor =>
            val results = for {
              fields <- c.keys.toList.map(_.toList)
              altResult <- alts.toList flatMap {
                case Alt(id, base, prism) =>
                  fields.contains(id).option(
                    c.downField(id).as(base).map(prism.upcast)
                  ).toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => Left(DecodingFailure(s"No fields found matching any of $altIds", c.history))
              case _ => Left(DecodingFailure(s"More than one matching field found among $altIds", c.history))
            }
          }

        case OneOfSchema(alts, Some(discriminatorField)) =>
          Decoder.instance { c: HCursor =>
            for {
              altId <- c.downField(discriminatorField).as[String]
              Alt(_, base, prism) <- alts.find(_.id == altId)
                .toRight(DecodingFailure(s"No '$discriminatorField' case of value '$altId'", c.history))
              altResult <- c.as(base).map(prism.upcast)
            } yield altResult
          }

        case RecordSchema(rb) =>
          decodeObj(rb)

        case IsoSchema(base, iso) =>
          base.map(iso.get)
      }
    }

  def annDecoderAlg[P[_]: FromJson, Ann[_]](implicit interpret: Ann ~> λ[T => Endo[Decoder[T]]]): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], Decoder] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], Decoder] {
      override def apply[I](s: HEnvT[Ann, SchemaF[P, *[_], *], Decoder, I]): Decoder[I] =
        interpret(s.ask).apply(decoderAlg[P].apply(s.fa))
    }

  def decodeObj[I](rb: FreeApplicative[PropSchema[I, Decoder, *], I]): Decoder[I] = {
    rb.foldMap(
      new (PropSchema[I, Decoder, *] ~> Decoder) {
        def apply[B](ps: PropSchema[I, Decoder, B]): Decoder[B] = ps match {
          case Required(field, base, _, None) =>
            Decoder.instance(_.downField(field).as(base))

          case Required(field, base, _, Some(default)) =>
            Decoder.instance(_.downField(field).as(base)).handleErrorWith(_ => Decoder.const(default))

          case opt: Optional[I, Decoder, i] =>
            Decoder.instance(_.downField(opt.fieldName).as[B](Decoder.decodeOption(opt.base)))

          case Constant(_, value, _) => Decoder.const(value)

          case abs: Absent[I, Decoder, i] =>
            Decoder.instance(_ => Option.empty[i].asRight[DecodingFailure])
        }
      }
    )
  }

  implicit def eitherKFromJson[P[_]: FromJson, Q[_]: FromJson]: FromJson[EitherK[P, Q, *]] = new FromJson[EitherK[P, Q, *]] {
    override val decoder: EitherK[P, Q, *] ~> Decoder = new (EitherK[P, Q, *] ~> Decoder) {
      override def apply[A](p: EitherK[P, Q, A]): Decoder[A] = {
        p.run.fold(
          FromJson[P].decoder(_),
          FromJson[Q].decoder(_),
        )
      }
    }
  }
}