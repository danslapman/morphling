package morphling.reactivemongo

import cats.*
import cats.data.EitherK
import cats.free.*
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import morphling.{Absent, Alt, Constant, HAlgebra, HEnvT, HFix, IsoSchema, OneOfSchema, Optional, PrimSchema, PropSchema, RecordSchema, Required, SchemaF}
import mouse.boolean.*
import mouse.option.*
import reactivemongo.api.bson.*
import simulacrum_.typeclass

@typeclass
trait FromBson[S[_]] {
  def reader: S ~> BSONReader
}

object FromBson {
  implicit class FromBsonOps[F[_], A](fa: F[A]) {
    def reader(implicit FB: FromBson[F]): BSONReader[A] = FB.reader(fa)
  }

  implicit def schemaFromBson[P[_]: FromBson]: FromBson[Schema[P, *]] = new FromBson[Schema[P, *]] {
    override val reader: Schema[P, *] ~> BSONReader = new (Schema[P, *] ~> BSONReader) {
      override def apply[I](schema: Schema[P, I]): BSONReader[I] =
        HFix.cataNT[SchemaF[P, *[_], *], BSONReader](decoderAlg[P]).apply(schema)
    }
  }

  implicit def annSchemaFromBson[P[_]: FromBson, A[_]: *[_] ~> λ[T => Endo[BSONReader[T]]]]
      : FromBson[AnnotatedSchema[P, A, *]] =
    new FromBson[AnnotatedSchema[P, A, *]] {
      override val reader: AnnotatedSchema[P, A, *] ~> BSONReader = new (AnnotatedSchema[P, A, *] ~> BSONReader) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): BSONReader[I] =
          HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], BSONReader](annDecoderAlg[P, A]).apply(schema)
      }
    }

  def decoderAlg[P[_]: FromBson]: HAlgebra[SchemaF[P, *[_], *], BSONReader] =
    new HAlgebra[SchemaF[P, *[_], *], BSONReader] {
      def apply[I](s: SchemaF[P, BSONReader, I]): BSONReader[I] = s match {
        case PrimSchema(p) =>
          FromBson[P].reader(p)

        case OneOfSchema(alts, None) =>
          BSONDocumentReader[I] { doc =>
            val results = for {
              fields <- doc.elements.map(_.name).toList
              altResult <- alts.toList flatMap { case Alt(id, base, prism) =>
                fields
                  .contains(id)
                  .option(
                    doc.getAsOpt(id)(base).map(prism.upcast)
                  )
                  .toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case Some(x) :: Nil => x
              case None :: Nil    => throw TypeDoesNotMatch(s"Could not deserialize ${alts.head.id}")
              case Nil            => throw DocumentKeyNotFound(s"No fields found matching any of $altIds")
              case _              => throw MultipleKeysFound(s"More than one matching field found among $altIds}")
            }
          }

        case OneOfSchema(alts, Some(discriminatorField)) =>
          BSONDocumentReader.from[I] { doc =>
            for {
              altId <- doc.getAsTry[String](discriminatorField)
              Alt(_, base, prism) <- alts
                .find(_.id == altId)
                .toTry(DocumentKeyNotFound(s"No '$discriminatorField' case of value '$altId'"))
              altResult <- doc.asTry(base).map(prism.upcast)
            } yield altResult
          }

        case RecordSchema(rb) =>
          decodeObj(rb)

        case IsoSchema(base, iso) =>
          base.afterRead(iso.get)
      }
    }

  def annDecoderAlg[P[_]: FromBson, Ann[_]](implicit
      interpret: Ann ~> λ[T => Endo[BSONReader[T]]]
  ): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], BSONReader] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], BSONReader] {
      override def apply[I](s: HEnvT[Ann, SchemaF[P, *[_], *], BSONReader, I]): BSONReader[I] =
        interpret(s.ask).apply(decoderAlg[P].apply(s.fa))
    }

  def decodeObj[I](rb: FreeApplicative[PropSchema[I, BSONReader, *], I]): BSONReader[I] = {
    implicit val djap: Applicative[BSONReader[*]] = new Applicative[BSONReader] {
      override def pure[T](a: T): BSONReader[T] = BSONReader[T](_ => a)

      override def ap[T, U](ff: BSONReader[T => U])(fa: BSONReader[T]): BSONReader[U] =
        (v: BSONValue) => ff.readTry(v).flatMap(fa.readTry(v).map(_))
    }

    rb.foldMap(
      new (PropSchema[I, BSONReader, *] ~> BSONReader) {
        def apply[B](ps: PropSchema[I, BSONReader, B]): BSONReader[B] = ps match {
          case Required(field, base, _, None) =>
            BSONDocumentReader[B](doc => doc.getAsOpt[B](field)(base).getOrElse(throw DocumentKeyNotFound(field)))

          case Required(field, base, _, Some(default)) =>
            BSONDocumentReader[B](doc => doc.getAsOpt[B](field)(base).getOrElse(default))

          case opt: Optional[I, BSONReader, i] =>
            BSONDocumentReader[B](doc => doc.getAsOpt[i](opt.fieldName)(opt.base))

          case Constant(_, value, _) =>
            BSONReader[B](_ => value)

          case abs: Absent[I, BSONReader, i] =>
            BSONReader(_ => Option.empty[i])
        }
      }
    )
  }

  implicit def eitherKFromBson[P[_]: FromBson, Q[_]: FromBson]: FromBson[EitherK[P, Q, *]] =
    new FromBson[EitherK[P, Q, *]] {
      override val reader = new (EitherK[P, Q, *] ~> BSONReader) {
        def apply[A](p: EitherK[P, Q, A]): BSONReader[A] =
          p.run.fold(
            FromBson[P].reader(_),
            FromBson[Q].reader(_),
          )
      }
    }
}
