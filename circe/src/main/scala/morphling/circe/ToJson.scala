package morphling.circe

import cats._
import cats.data.{EitherK, State}
import cats.data.State._
import cats.free._
import io.circe.{Encoder, Json, JsonObject}
import io.circe.syntax._
import morphling._
import morphling.HFunctor._
import morphling.Schema._
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option._
import simulacrum.typeclass

@typeclass
trait ToJson[S[_]] {
  def encoder: S ~> Encoder
}

object ToJson {
  implicit class ToJsonOps[F[_], A](private val fa: F[A]) {
    def encoder(implicit TJ: ToJson[F]): Encoder[A] = TJ.encoder(fa)
  }

  implicit def schemaToJson[P[_]: ToJson]: ToJson[Schema[P, *]] = new ToJson[Schema[P, *]] {
    val encoder: Schema[P, *] ~> Encoder = new (Schema[P, *] ~> Encoder) {
      override def apply[I](schema: Schema[P, I]): Encoder[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], Encoder](serializeAlg).apply(schema)
      }
    }
  }

  implicit def annSchemaToJson[P[_]: ToJson, A[_]]: ToJson[AnnotatedSchema[P, A, *]] = new ToJson[AnnotatedSchema[P, A, *]] {
    val encoder: AnnotatedSchema[P, A, *] ~> Encoder = new (AnnotatedSchema[P, A, *] ~> Encoder) {
      override def apply[I](schema: AnnotatedSchema[P, A, I]): Encoder[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], Encoder](serializeAlg).apply(
          HFix.forget[SchemaF[P, *[_], *], A].apply(schema)
        )
      }
    }
  }

  def serializeAlg[P[_]: ToJson]: HAlgebra[SchemaF[P, *[_], *], Encoder] =
    new HAlgebra[SchemaF[P, *[_], *], Encoder] {
      def apply[I](schema: SchemaF[P, Encoder, I]): Encoder[I] = {
        schema match {
          case s: PrimSchema[P, Encoder, I] => ToJson[P].encoder(s.prim)

          case s: OneOfSchema[P, Encoder, I] =>
            (value: I) => {
              s.discriminator.cata(
                discriminator =>
                  s.alts.map { case alt @ Alt(id, base, prism) =>
                    prism.getOption(value).map(alt.base(_).mapObject((discriminator := alt.id) +: _))
                  }.collect { case Some(json) => json }.head,
                s.alts.map { case Alt(id, base, prism) =>
                  prism.getOption(value).map(base(_)).map(json => Json.obj(id -> json))
                }.collect { case Some(json) => json }.head
              )
            }

          case s: RecordSchema[P, Encoder, I] =>
            serializeObjF[P, I](s.props)

          case s: IsoSchema[P, Encoder, i0, I] =>
            s.base.contramap(s.iso.reverseGet(_))
        }
      }
    }

  def serializeObjF[P[_]: ToJson, I](rb: FreeApplicative[PropSchema[I, Encoder, *], I]): Encoder[I] = {
    (value: I) => Json.fromJsonObject(
      rb.foldMap[State[JsonObject, *]](
        new (PropSchema[I, Encoder, *] ~> State[JsonObject, *]) {
          def apply[B](ps: PropSchema[I, Encoder, B]): State[JsonObject, B] = {
            for {
              _ <- modify { (obj: JsonObject) =>
                ps match {
                  case req: Required[I, Encoder, i] =>
                    (req.fieldName, req.base(req.getter.get(value))) +: obj

                  case opt: Optional[I, Encoder, i] =>
                    opt.getter.get(value).cata(v => (opt.fieldName, opt.base(v)) +: obj, obj)

                  case Constant(_, _, _) => obj

                  case Absent(_, _) => obj
                }
              }
            } yield ps.getter.get(value)
          }
        }
      ).runS(JsonObject.empty).value
    )
  }

  implicit def eitherKToJson[P[_]: ToJson, Q[_]: ToJson]: ToJson[EitherK[P, Q, *]] =
    new ToJson[EitherK[P, Q, *]] {
      val encoder = new (EitherK[P, Q, *] ~> Encoder) {
        def apply[A](p: EitherK[P, Q, A]): Encoder[A] = {
          p.run.fold(ToJson[P].encoder(_), ToJson[Q].encoder(_))
        }
      }
    }
}