package morphling.reactivemongo

import scala.annotation.implicitNotFound
import scala.util.Success
import scala.util.Try

import cats.*
import cats.data.EitherK
import cats.data.State
import cats.data.State.*
import cats.free.*
import morphling.Absent
import morphling.Alt
import morphling.Constant
import morphling.HAlgebra
import morphling.HFix
import morphling.IsoSchema
import morphling.OneOfSchema
import morphling.Optional
import morphling.PrimSchema
import morphling.PropSchema
import morphling.RecordSchema
import morphling.Required
import morphling.Schema.*
import morphling.SchemaF
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import reactivemongo.api.bson.*
import simulacrum.typeclass

@implicitNotFound("Could not find an instance of ToBson for ${S}")
@typeclass
trait ToBson[S[_]] extends Serializable {
  def writer: S ~> BSONWriter

  extension [F[_], A](fa: F[A])(using TB: ToBson[F]) {
    def writer: BSONWriter[A] = TB.writer(fa)
  }
}

object ToBson {
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

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ToBson]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: ToBson[S]): ToBson[S] = instance

  object ops {
    implicit def toAllToBsonOps[S[_], A](target: S[A])(implicit tc: ToBson[S]): AllOps[S, A] {
      type TypeClassType = ToBson[S]
    } = new AllOps[S, A] {
      type TypeClassType = ToBson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: ToBson[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToToBsonOps extends Serializable {
    implicit def toToBsonOps[S[_], A](target: S[A])(implicit tc: ToBson[S]): Ops[S, A] {
      type TypeClassType = ToBson[S]
    } = new Ops[S, A] {
      type TypeClassType = ToBson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToToBsonOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
