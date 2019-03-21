package morphling.reactivemongo

import cats._
import cats.data.{EitherK, State}
import cats.data.State._
import cats.free._
import morphling.{Alt, HFix, IsoSchema, OneOfSchema, Optional, PrimSchema, PropSchema, RecordSchema, Required, SchemaF}
import morphling.HFunctor._
import morphling.Schema._
import mouse.option._
import reactivemongo.bson._
import simulacrum.typeclass

@typeclass
trait ToBson[S[_]] {
  def serialize: S ~> (? => BSONValue)
}

object ToBson {
  implicit class ToBsonOps[F[_], A](fa: F[A]) {
    def toJson(a: A)(implicit TB: ToBson[F]): BSONValue = TB.serialize(fa)(a)
  }

  implicit def schemaToBson[P[_]: ToBson]: ToBson[Schema[P, ?]] = new ToBson[Schema[P, ?]] {
    def serialize = new (Schema[P, ?] ~> (? => BSONValue)) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], ? => BSONValue](serializeAlg).apply(schema)
      }
    }
  }

  def serializeAlg[P[_]: ToBson]: HAlgebra[SchemaF[P, ?[_], ?], ? => BSONValue] =
    new HAlgebra[SchemaF[P, ?[_], ?], ? => BSONValue] {
      def apply[I](schema: SchemaF[P, ? => BSONValue, I]): I => BSONValue = {
        schema match {
          case s: PrimSchema[P, ? => BSONValue, I] =>
            ToBson[P].serialize(s.prim)

          case s: OneOfSchema[P, ? => BSONValue, I] =>
            (value: I) => {
              val results = s.alts.toList flatMap {
                case alt: Alt[? => BSONValue, I, i] => {
                  alt.prism.getOption(value).map(alt.base).toList map { bson =>
                    document(alt.id -> bson)
                  }
                }
              }

              results.head //yeah, I know
            }

          case s: RecordSchema[P, ? => BSONValue, I] =>
            serializeObjF[P, I](s.props)

          case s: IsoSchema[P, ? => BSONValue, i0, I] =>
            s.base.compose(s.iso.reverseGet(_))
        }
      }
    }

  def serializeObjF[P[_]: ToBson, I](rb: FreeApplicative[PropSchema[I, ? => BSONValue, ?], I]): I => BSONValue = {
    (value: I) => document(
      rb.foldMap[State[BSONDocument, ?]](
        new (PropSchema[I, ? => BSONValue, ?] ~> State[BSONDocument, ?]) {
          def apply[B](ps: PropSchema[I, ? => BSONValue, B]): State[BSONDocument, B] = {
            for {
              _ <- modify { (doc: BSONDocument) =>
                ps match {
                  case req: Required[I, ? => BSONValue, i] =>
                    (req.fieldName, req.base(req.getter.get(value))) ~: doc

                  case opt: Optional[I, ? => BSONValue, i] =>
                    opt.getter.get(value).cata(v => (opt.fieldName, opt.base(v)) ~: doc, doc)
                }
              }
            } yield ps.getter.get(value)
          }
        }
      ).run(document).value._1
    )
  }

  implicit def eitherKToBson[P[_]: ToBson, Q[_]: ToBson] = new ToBson[EitherK[P, Q, ?]] {
    val serialize = new (EitherK[P, Q, ?] ~> (? => BSONValue)) {
      def apply[A](p: EitherK[P, Q, A]): A => BSONValue = {
        p.run.fold(
          ToBson[P].serialize(_),
          ToBson[Q].serialize(_)
        )
      }
    }
  }
}