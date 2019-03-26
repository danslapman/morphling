package morphling.circe

import cats._
import cats.data.{EitherK, State}
import cats.data.State._
import cats.free._
import io.circe.{Json, JsonObject}
import morphling._
import morphling.HFunctor._
import morphling.Schema._
import mouse.option._
import simulacrum.typeclass

@typeclass
trait ToJson[S[_]] {
  def serialize: S ~> (? => Json)
}

object ToJson {
  implicit class ToJsonOps[F[_], A](fa: F[A]) {
    def toJson(a: A)(implicit TJ: ToJson[F]): Json = TJ.serialize(fa)(a)
  }

  implicit def schemaToJson[P[_]: ToJson]: ToJson[Schema[P, ?]] = new ToJson[Schema[P, ?]] {
    def serialize = new (Schema[P, ?] ~> (? => Json)) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], ? => Json](serializeAlg).apply(schema)
      }
    }
  }

  def serializeAlg[P[_]: ToJson]: HAlgebra[SchemaF[P, ?[_], ?], ? => Json] =
    new HAlgebra[SchemaF[P, ?[_], ?], ? => Json] {
      def apply[I](schema: SchemaF[P, ? => Json, I]): I => Json = {
        schema match {
          case s: PrimSchema[P, ? => Json, I] => ToJson[P].serialize(s.prim)

          case s: OneOfSchema[P, ? => Json, I] =>
            (value: I) => {
              val results = s.alts.toList flatMap {
                case alt: Alt[? => Json, I, i] => {
                  alt.prism.getOption(value).map(alt.base).toList map { json =>
                    Json.fromJsonObject(JsonObject.singleton(alt.id, json))
                  }
                }
              }

              results.head //yeah, I know
            }

          case s: RecordSchema[P, ? => Json, I] =>
            serializeObjF[P, I](s.props)

          case s: IsoSchema[P, ? => Json, i0, I] =>
            s.base.compose(s.iso.reverseGet(_))
        }
      }
    }

  def serializeObjF[P[_]: ToJson, I](rb: FreeApplicative[PropSchema[I, ? => Json, ?], I]): I => Json = {
    (value: I) => Json.fromJsonObject(
      rb.foldMap[State[JsonObject, ?]](
        new (PropSchema[I, ? => Json, ?] ~> State[JsonObject, ?]) {
          def apply[B](ps: PropSchema[I, ? => Json, B]): State[JsonObject, B] = {
            for {
              _ <- modify { (obj: JsonObject) =>
                ps match {
                  case req: Required[I, ? => Json, i] =>
                    (req.fieldName, req.base(req.getter.get(value))) +: obj

                  case opt: Optional[I, ? => Json, i] =>
                    opt.getter.get(value).cata(v => (opt.fieldName, opt.base(v)) +: obj, obj)
                }
              }
            } yield ps.getter.get(value)
          }
        }
      ).runS(JsonObject.empty).value
    )
  }

  implicit def eitherKToJson[P[_]: ToJson, Q[_]: ToJson] = new ToJson[EitherK[P, Q, ?]] {
    val serialize = new (EitherK[P, Q, ?] ~> (? => Json)) {
      def apply[A](p: EitherK[P, Q, A]): A => Json = {
        p.run.fold(
          ToJson[P].serialize(_),
          ToJson[Q].serialize(_)
        )
      }
    }
  }
}