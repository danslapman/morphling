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

import scala.util.Success

@typeclass
trait ToBson[S[_]] {
  def writer: S ~> BSONWriter[?, BSONValue]
}

object ToBson {
  implicit class ToBsonOps[F[_], A](fa: F[A]) {
    def writer(implicit TB: ToBson[F]): BSONWriter[A, BSONValue] = TB.writer(fa)
  }

  implicit def schemaToBson[P[_]: ToBson]: ToBson[Schema[P, ?]] = new ToBson[Schema[P, ?]] {
    val writer: Schema[P, ?] ~> BSONWriter[?, BSONValue] = new (Schema[P, ?] ~> BSONWriter[?, BSONValue]) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], BSONWriter[?, BSONValue]](serializeAlg).apply(schema)
      }
    }
  }

  def serializeAlg[P[_]: ToBson]: HAlgebra[SchemaF[P, ?[_], ?], BSONWriter[?, BSONValue]] =
    new HAlgebra[SchemaF[P, ?[_], ?], BSONWriter[?, BSONValue]] {
      def apply[I](schema: SchemaF[P, BSONWriter[?, BSONValue], I]): BSONWriter[I, BSONValue] = {
        schema match {
          case s: PrimSchema[P, BSONWriter[?, BSONValue], I] =>
            ToBson[P].writer(s.prim)

          case s: OneOfSchema[P, BSONWriter[?, BSONValue], I] =>
            (value: I) => {
              s.discriminator.cata(
                dField => {
                  s.alts.map { case alt: Alt[BSONWriter[?, BSONValue], I, i] =>
                    alt.prism.getOption(value).map(v =>
                      alt.base.write(v) match {
                        case BSONDocument(elems) =>
                          BSONDocument(Success(BSONElement(dField, BSONString(alt.id))) #:: elems)
                        case other => other
                      }
                  )}.collect { case Some(doc) => doc}.head
                },
                s.alts.map { case alt: Alt[BSONWriter[?, BSONValue], I, i] =>
                  alt.prism.getOption(value).map(alt.base.write(_)).map(bson => document(alt.id -> bson))
                }.collect { case Some(bson) => bson }.head
              )
            }

          case s: RecordSchema[P, BSONWriter[?, BSONValue], I] =>
            serializeObjF[P, I](s.props).asInstanceOf[BSONWriter[I, BSONValue]]

          case s: IsoSchema[P, BSONWriter[?, BSONValue], i0, I] =>
            s.base.beforeWrite(s.iso.reverseGet(_))
        }
      }
    }

  def serializeObjF[P[_]: ToBson, I](rb: FreeApplicative[PropSchema[I, BSONWriter[?, BSONValue], ?], I]): BSONDocumentWriter[I] = {
    (value: I) => document(
      rb.foldMap[State[BSONDocument, ?]](
        new (PropSchema[I, BSONWriter[?, BSONValue], ?] ~> State[BSONDocument, ?]) {
          def apply[B](ps: PropSchema[I, BSONWriter[?, BSONValue], B]): State[BSONDocument, B] = {
            for {
              _ <- modify { (doc: BSONDocument) =>
                ps match {
                  case req: Required[I, BSONWriter[?, BSONValue], i] =>
                    (req.fieldName, req.base.write(req.getter.get(value))) ~: doc

                  case opt: Optional[I, BSONWriter[?, BSONValue], i] =>
                    opt.getter.get(value).cata(v => (opt.fieldName, opt.base.write(v)) ~: doc, doc)
                }
              }
            } yield ps.getter.get(value)
          }
        }
      ).runS(document).value
    )
  }

  implicit def eitherKToBson[P[_]: ToBson, Q[_]: ToBson]: ToBson[EitherK[P, Q, ?]] =
    new ToBson[EitherK[P, Q, ?]] {
      val writer = new (EitherK[P, Q, ?] ~> BSONWriter[?, BSONValue]) {
        def apply[A](p: EitherK[P, Q, A]): BSONWriter[A, BSONValue] = {
          p.run.fold(
            ToBson[P].writer(_),
            ToBson[Q].writer(_)
          )
        }
      }
    }
}