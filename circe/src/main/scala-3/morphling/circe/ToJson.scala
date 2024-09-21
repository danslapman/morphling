package morphling.circe

import cats.*
import cats.data.State.*
import cats.data.{EitherK, State}
import cats.free.*
import io.circe.syntax.*
import io.circe.{Encoder, Json, JsonObject}
import morphling.*
import morphling.Schema.*
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*

trait ToJson[S[_]] extends Serializable {
  def encoder: S ~> Encoder

  extension [F[_], A](fa: F[A])(using TJ: ToJson[F]) def encoder: Encoder[A] = TJ.encoder(fa)
}

object ToJson {
  def apply[P[_]](using tj: ToJson[P]): ToJson[P] = tj

  given [P[_]: ToJson]: ToJson[Schema[P, _]] =
    new ToJson[Schema[P, _]] {
      override val encoder: Schema[P, _] ~> Encoder = new (Schema[P, _] ~> Encoder) {
        override def apply[I](schema: Schema[P, I]): Encoder[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], Encoder](serializeAlg).apply(schema)
      }
    }

  given [P[_]: ToJson, A[_]]: ToJson[AnnotatedSchema[P, A, _]] =
    new ToJson[AnnotatedSchema[P, A, _]] {
      override val encoder: AnnotatedSchema[P, A, _] ~> Encoder = new (AnnotatedSchema[P, A, _] ~> Encoder) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): Encoder[I] =
          HFix
            .cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], Encoder](serializeAlg)
            .apply(
              HFix.forget[[Y[_], Z] =>> SchemaF[P, Y, Z], A].apply(schema)
            )
      }
    }

  def serializeAlg[P[_]: ToJson]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Encoder] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Encoder] {
      def apply[I](schema: SchemaF[P, Encoder, I]): Encoder[I] =
        schema match {
          case s: PrimSchema[P, Encoder, I] => ToJson[P].encoder(s.prim)

          case s: OneOfSchema[P, Encoder, I] =>
            (value: I) =>
              s.discriminator.cata(
                discriminator =>
                  s.alts
                    .map { case alt @ Alt(id, base, prism) =>
                      prism.getOption(value).map(alt.base(_).mapObject((discriminator := alt.id) +: _))
                    }
                    .collect { case Some(json) => json }
                    .head,
                s.alts
                  .map { case Alt(id, base, prism) =>
                    prism.getOption(value).map(base(_)).map(json => Json.obj(id -> json))
                  }
                  .collect { case Some(json) => json }
                  .head
              )

          case s: RecordSchema[P, Encoder, I] =>
            serializeObjF[P, I](s.props)

          case s: IsoSchema[P, Encoder, i0, I] =>
            s.base.contramap(s.eqv.upcast(_))
        }
    }

  def serializeObjF[P[_]: ToJson, I](rb: FreeApplicative[PropSchema[I, Encoder, _], I]): Encoder[I] = { (value: I) =>
    Json.fromJsonObject(
      rb.foldMap[State[JsonObject, _]](
        new (PropSchema[I, Encoder, _] ~> State[JsonObject, _]) {
          def apply[B](ps: PropSchema[I, Encoder, B]): State[JsonObject, B] =
            for {
              _ <- modify { (obj: JsonObject) =>
                ps match {
                  case req: Required[I, Encoder, i] =>
                    (req.fieldName, req.base(req.extract.extract(value))) +: obj

                  case opt: Optional[I, Encoder, i] @unchecked =>
                    opt.extract.extract(value).cata(v => (opt.fieldName, opt.base(v)) +: obj, obj)

                  case Constant(_, _, _) => obj

                  case Absent(_, _) => obj
                }
              }
            } yield ps.extract.extract(value)
        }
      ).runS(JsonObject.empty)
        .value
    )
  }

  given [P[_]: ToJson, Q[_]: ToJson]: ToJson[EitherK[P, Q, _]] =
    new ToJson[EitherK[P, Q, _]] {
      override val encoder = new (EitherK[P, Q, _] ~> Encoder) {
        def apply[A](p: EitherK[P, Q, A]): Encoder[A] =
          p.run.fold(ToJson[P].encoder(_), ToJson[Q].encoder(_))
      }
    }
}
