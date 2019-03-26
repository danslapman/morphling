package morphling.circe

import cats._
import cats.data.EitherK
import cats.free._
import io.circe.{Decoder, DecodingFailure, HCursor, Json}
import morphling._
import morphling.HFunctor._
import morphling.Schema._
import mouse.boolean._
import simulacrum.typeclass

@typeclass
trait FromJson[S[_]] {
  def decoder: S ~> Decoder
}

object FromJson {
  implicit class FromJsonOps[F[_], A](fa: F[A]) {
    def fromJson(a: Json)(implicit FJ: FromJson[F]): Decoder.Result[A] = {
      FJ.decoder(fa).decodeJson(a)
    }
  }

  implicit def schemaFromJson[P[_]: FromJson]: FromJson[Schema[P, ?]] = new FromJson[Schema[P, ?]] {
    def decoder = new (Schema[P, ?] ~> Decoder) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], Decoder](decoderAlg[P]).apply(schema)
      }
    }
  }

  def decoderAlg[P[_]: FromJson]: HAlgebra[SchemaF[P, ?[_], ?], Decoder] =
    new HAlgebra[SchemaF[P, ?[_], ?], Decoder] {
      def apply[I](s: SchemaF[P, Decoder, I]): Decoder[I] = s match {
        case PrimSchema(p) => FromJson[P].decoder(p)

        case OneOfSchema(alts) =>
          Decoder.instance { (c: HCursor) =>
            val results = for {
              fields <- c.keys.toList.map(_.toList)
              altResult <- alts.toList flatMap {
                case Alt(id, base, prism) =>
                  fields.contains(id).option(
                    c.downField(id).as(base).map(prism.reverseGet)
                  ).toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => Left(DecodingFailure(s"No fields found matching any of $altIds", c.history))
              case _ => Left(DecodingFailure(s"More than one matching field found among $altIds}", c.history))
            }
          }

        case RecordSchema(rb) =>
          decodeObj(rb)

        case IsoSchema(base, iso) =>
          base.map(iso.get)
      }
    }

  def decodeObj[I](rb: FreeApplicative[PropSchema[I, Decoder, ?], I]): Decoder[I] = {
    implicit val djap: Applicative[Decoder] = new Applicative[Decoder] {
      override def pure[T](a: T) = Decoder(_ => Right(a))

      override def ap[T, U](ff: Decoder[T => U])(fa: Decoder[T]): Decoder[U] =
        fa.flatMap(a => ff.map(_(a)))
    }

    rb.foldMap(
      new (PropSchema[I, Decoder, ?] ~> Decoder) {
        def apply[B](ps: PropSchema[I, Decoder, B]): Decoder[B] = ps match {
          case Required(field, base, _, _) =>
            Decoder(_.downField(field).as(base))

          case opt: Optional[I, Decoder, i] =>
            Decoder(_.downField(opt.fieldName).as[B](Decoder.decodeOption(opt.base)))
        }
      }
    )
  }

  implicit def eitherKFromJson[P[_]: FromJson, Q[_]: FromJson] = new FromJson[EitherK[P, Q, ?]] {
    val decoder = new (EitherK[P, Q, ?] ~> Decoder) {
      def apply[A](p: EitherK[P, Q, A]): Decoder[A] = {
        p.run.fold(
          FromJson[P].decoder(_),
          FromJson[Q].decoder(_),
        )
      }
    }
  }
}