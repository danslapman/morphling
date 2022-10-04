package morphling.reactivemongo

import scala.annotation.implicitNotFound

import cats.*
import cats.data.EitherK
import cats.free.*
import morphling.Absent
import morphling.Alt
import morphling.Constant
import morphling.HAlgebra
import morphling.HEnvT
import morphling.HFix
import morphling.IsoSchema
import morphling.OneOfSchema
import morphling.Optional
import morphling.PrimSchema
import morphling.PropSchema
import morphling.RecordSchema
import morphling.Required
import morphling.Schema.Schema
import morphling.SchemaF
import morphling.annotated.Schema.AnnotatedSchema
import morphling.given
import mouse.boolean.*
import mouse.option.*
import reactivemongo.api.bson.*
import simulacrum.typeclass

@implicitNotFound("Could not find an instance of FromBson for ${S}")
@typeclass
trait FromBson[S[_]] extends Serializable {
  def reader: S ~> BSONReader

  extension [F[_], A](fa: F[A])(using FB: FromBson[F]) {
    def reader: BSONReader[A] = FB.reader(fa)
  }
}

object FromBson {
  given [P[_]: FromBson]: FromBson[Schema[P, _]] =
    new FromBson[Schema[P, _]] {
      override val reader: Schema[P, _] ~> BSONReader = new (Schema[P, _] ~> BSONReader) {
        override def apply[I](schema: Schema[P, I]): BSONReader[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONReader](decoderAlg[P]).apply(schema)
      }
    }

  given [P[_]: FromBson, A[_]: [Y[_]] =>> Y ~> ([T] =>> Endo[BSONReader[T]])]: FromBson[AnnotatedSchema[P, A, _]] =
    new FromBson[AnnotatedSchema[P, A, _]] {
      override val reader: AnnotatedSchema[P, A, *] ~> BSONReader = new (AnnotatedSchema[P, A, _] ~> BSONReader) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): BSONReader[I] =
          HFix
            .cataNT[[Y1[_], Z1] =>> HEnvT[A, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], BSONReader](annDecoderAlg[P, A])
            .apply(schema)
      }
    }

  def decoderAlg[P[_]: FromBson]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONReader] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], BSONReader] {
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
      interpret: Ann ~> ([T] =>> Endo[BSONReader[T]])
  ): HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], BSONReader] =
    new HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], BSONReader] {
      override def apply[I](s: HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], BSONReader, I]): BSONReader[I] =
        interpret(s.ask).apply(decoderAlg[P].apply(s.fa))
    }

  def decodeObj[I](rb: FreeApplicative[PropSchema[I, BSONReader, _], I]): BSONReader[I] = {
    given Applicative[BSONReader[_]] = new Applicative[BSONReader] {
      override def pure[T](a: T): BSONReader[T] = BSONReader[T](_ => a)

      override def ap[T, U](ff: BSONReader[T => U])(fa: BSONReader[T]): BSONReader[U] =
        (v: BSONValue) => ff.readTry(v).flatMap(fa.readTry(v).map(_))
    }

    rb.foldMap(
      new (PropSchema[I, BSONReader, _] ~> BSONReader) {
        def apply[B](ps: PropSchema[I, BSONReader, B]): BSONReader[B] = ps match {
          case Required(field, base, _, None) =>
            BSONDocumentReader[B](doc => doc.getAsOpt[B](field)(base).getOrElse(throw DocumentKeyNotFound(field)))

          case Required(field, base, _, Some(default)) =>
            BSONDocumentReader[B](doc => doc.getAsOpt[B](field)(base).getOrElse(default))

          case opt: Optional[I, BSONReader, i] @unchecked =>
            BSONDocumentReader[B](doc => doc.getAsOpt[i](opt.fieldName)(opt.base))

          case Constant(_, value, _) =>
            BSONReader[B](_ => value)

          case abs: Absent[I, BSONReader, i] @unchecked =>
            BSONReader(_ => Option.empty[i])
        }
      }
    )
  }

  given [P[_]: FromBson, Q[_]: FromBson]: FromBson[EitherK[P, Q, _]] =
    new FromBson[EitherK[P, Q, _]] {
      override val reader = new (EitherK[P, Q, _] ~> BSONReader) {
        def apply[A](p: EitherK[P, Q, A]): BSONReader[A] =
          p.run.fold(
            FromBson[P].reader(_),
            FromBson[Q].reader(_),
          )
      }
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[FromBson]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: FromBson[S]): FromBson[S] = instance

  object ops {
    implicit def toAllFromBsonOps[S[_], A](target: S[A])(implicit tc: FromBson[S]): AllOps[S, A] {
      type TypeClassType = FromBson[S]
    } = new AllOps[S, A] {
      type TypeClassType = FromBson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: FromBson[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToFromBsonOps extends Serializable {
    implicit def toFromBsonOps[S[_], A](target: S[A])(implicit tc: FromBson[S]): Ops[S, A] {
      type TypeClassType = FromBson[S]
    } = new Ops[S, A] {
      type TypeClassType = FromBson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToFromBsonOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
