package morphling.reactivemongo

import cats.*
import cats.data.State.*
import cats.data.{EitherK, State}
import cats.free.*
import morphling.Schema.*
import morphling.annotated.Schema.AnnotatedSchema
import morphling.{Absent, Alt, Constant, HAlgebra, HFix, IsoSchema, OneOfSchema, Optional, PrimSchema, PropSchema, RecordSchema, Required, SchemaF}
import mouse.option.*
import reactivemongo.api.bson.*

import scala.util.{Success, Try}

trait ToBson[S[_]] extends Serializable {
  def writer: S ~> BSONWriter

  extension [F[_], A](fa: F[A])(using TB: ToBson[F]) {
    def writer: BSONWriter[A] = TB.writer(fa)
  }
}

object ToBson {
  def apply[P[_]](using tb: ToBson[P]): ToBson[P] = tb

  given [P[_]: ToBson]: ToBson[Schema[P, _]] =
    new ToBson[Schema[P, _]] {
      override val writer: Schema[P, _] ~> BSONWriter = new (Schema[P, _] ~> BSONWriter) {
        override def apply[I](schema: Schema[P, I]): BSONWriter[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONWriter](serializeAlg).apply(schema)
      }
    }

  given [P[_]: ToBson, A[_]]: ToBson[AnnotatedSchema[P, A, _]] =
    new ToBson[AnnotatedSchema[P, A, _]] {
      override val writer: AnnotatedSchema[P, A, _] ~> BSONWriter = new (AnnotatedSchema[P, A, _] ~> BSONWriter) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): BSONWriter[I] =
          HFix
            .cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONWriter](serializeAlg)
            .apply(
              HFix.forget[[Y[_], Z] =>> SchemaF[P, Y, Z], A].apply(schema)
            )
      }
    }

  def serializeAlg[P[_]: ToBson]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONWriter] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONWriter] {
      def apply[I](schema: SchemaF[P, BSONWriter, I]): BSONWriter[I] =
        schema match {
          case s: PrimSchema[P, BSONWriter, I] =>
            ToBson[P].writer(s.prim)

          case s: OneOfSchema[P, BSONWriter, I] =>
            (value: I) =>
              s.discriminator.cata(
                dField =>
                  s.alts
                    .map { case alt: Alt[BSONWriter, I, i] =>
                      alt.subset
                        .getOption(value)
                        .map(v =>
                          alt.base.writeTry(v).map {
                            case BSONDocument(elems) =>
                              BSONDocument((BSONElement(dField, BSONString(alt.id)) +: elems)*)
                            case other => other
                          }
                        )
                    }
                    .collect { case Some(doc) => doc }
                    .head,
                Success(
                  s.alts
                    .map { case alt: Alt[BSONWriter, I, i] =>
                      alt.subset.getOption(value).flatMap(alt.base.writeOpt(_)).map(bson => document(alt.id -> bson))
                    }
                    .collect { case Some(bson) => bson }
                    .head
                )
              )

          case s: RecordSchema[P, BSONWriter, I] =>
            serializeObjF[P, I](s.props).asInstanceOf[BSONWriter[I]]

          case s: IsoSchema[P, BSONWriter, i0, I] =>
            s.base.beforeWrite(s.eqv.upcast(_))
        }
    }

  def serializeObjF[P[_]: ToBson, I](rb: FreeApplicative[PropSchema[I, BSONWriter, _], I]): BSONDocumentWriter[I] = {
    (value: I) =>
      rb.foldMap[State[Try[BSONDocument], _]](
        new (PropSchema[I, BSONWriter, _] ~> State[Try[BSONDocument], _]) {
          def apply[B](ps: PropSchema[I, BSONWriter, B]): State[Try[BSONDocument], B] =
            for {
              _ <- modify { (tryDoc: Try[BSONDocument]) =>
                tryDoc.flatMap { (doc: BSONDocument) =>
                  ps match {
                    case req: Required[I, BSONWriter, i] =>
                      req.base.writeTry(req.extract.extract(value)).map(doc ++ BSONElement(req.fieldName, _))

                    case opt: Optional[I, BSONWriter, i] @unchecked =>
                      opt.extract
                        .extract(value)
                        .cata(
                          v =>
                            opt.base
                              .writeTry(v)
                              .map(doc ++ BSONElement(opt.fieldName, _)),
                          Success(doc)
                        )

                    case const: Constant[I, BSONWriter, i] => Success(doc)

                    case abs: Absent[I, BSONWriter, i] @unchecked => Success(doc)
                  }
                }
              }
            } yield ps.extract.extract(value)
        }
      ).runS(Success(document))
        .value
  }

  given [P[_]: ToBson, Q[_]: ToBson]: ToBson[EitherK[P, Q, _]] =
    new ToBson[EitherK[P, Q, _]] {
      override val writer = new (EitherK[P, Q, _] ~> BSONWriter) {
        def apply[A](p: EitherK[P, Q, A]): BSONWriter[A] =
          p.run.fold(
            ToBson[P].writer(_),
            ToBson[Q].writer(_)
          )
      }
    }
}
