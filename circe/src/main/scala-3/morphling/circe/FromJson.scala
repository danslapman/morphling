package morphling.circe

import scala.annotation.implicitNotFound

import cats.*
import cats.data.EitherK
import cats.free.*
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import morphling.*
import morphling.Schema.*
import morphling.annotated.Schema.AnnotatedSchema
import morphling.given
import mouse.boolean.*
import simulacrum.typeclass

@implicitNotFound("Could not find an instance of FromJson for ${S}")
@typeclass
trait FromJson[S[_]] extends Serializable {
  def decoder: S ~> Decoder

  extension [F[_], A](fa: F[A])(using FJ: FromJson[F]) {
    def decoder: Decoder[A] = FJ.decoder(fa)
  }
}

object FromJson {

  given [P[_]: FromJson]: FromJson[Schema[P, _]] =
    new FromJson[Schema[P, _]] {
      override val decoder: Schema[P, _] ~> Decoder = new (Schema[P, _] ~> Decoder) {
        override def apply[I](schema: Schema[P, I]): Decoder[I] =
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], Decoder](decoderAlg[P]).apply(schema)
      }
    }

  given [P[_]: FromJson, A[_]: [Y[_]] =>> Y ~> ([T] =>> Endo[Decoder[T]])]: FromJson[AnnotatedSchema[P, A, *]] =
    new FromJson[AnnotatedSchema[P, A, _]] {
      override val decoder: AnnotatedSchema[P, A, _] ~> Decoder = new (AnnotatedSchema[P, A, _] ~> Decoder) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): Decoder[I] =
          HFix
            .cataNT[[Y1[_], Z1] =>> HEnvT[A, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Decoder](annDecoderAlg[P, A])
            .apply(schema)
      }
    }

  def decoderAlg[P[_]: FromJson]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Decoder] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], Decoder] {
      def apply[I](s: SchemaF[P, Decoder, I]): Decoder[I] = s match {
        case PrimSchema(p) => FromJson[P].decoder(p)

        case OneOfSchema(alts, None) =>
          Decoder.instance { (c: HCursor) =>
            val results = for {
              fields <- c.keys.toList.map(_.toList)
              altResult <- alts.toList flatMap { case Alt(id, base, prism) =>
                fields
                  .contains(id)
                  .option(
                    c.downField(id).as(base).map(prism.upcast)
                  )
                  .toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil      => Left(DecodingFailure(s"No fields found matching any of $altIds", c.history))
              case _        => Left(DecodingFailure(s"More than one matching field found among $altIds", c.history))
            }
          }

        case OneOfSchema(alts, Some(discriminatorField)) =>
          Decoder.instance { (c: HCursor) =>
            for {
              altId <- c.downField(discriminatorField).as[String]
              Alt(_, base, prism) <- alts
                .find(_.id == altId)
                .toRight(DecodingFailure(s"No '$discriminatorField' case of value '$altId'", c.history))
              altResult <- c.as(base).map(prism.upcast)
            } yield altResult
          }

        case RecordSchema(rb) =>
          decodeObj(rb)

        case IsoSchema(base, iso) =>
          base.map(iso.get)
      }
    }

  def annDecoderAlg[P[_]: FromJson, Ann[_]](implicit
      interpret: Ann ~> ([T] =>> Endo[Decoder[T]])
  ): HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Decoder] =
    new HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], Decoder] {
      override def apply[I](s: HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Decoder, I]): Decoder[I] =
        interpret(s.ask).apply(decoderAlg[P].apply(s.fa))
    }

  def decodeObj[I](rb: FreeApplicative[PropSchema[I, Decoder, _], I]): Decoder[I] =
    rb.foldMap(
      new (PropSchema[I, Decoder, _] ~> Decoder) {
        def apply[B](ps: PropSchema[I, Decoder, B]): Decoder[B] = ps match {
          case Required(field, base, _, None) =>
            Decoder.instance(_.downField(field).as(base))

          case Required(field, base, _, Some(default)) =>
            Decoder.instance(_.downField(field).as(base)).handleErrorWith(_ => Decoder.const(default))

          case opt: Optional[I, Decoder, i] @unchecked =>
            Decoder.instance(_.downField(opt.fieldName).as[B](Decoder.decodeOption(opt.base)))

          case Constant(_, value, _) => Decoder.const(value)

          case abs: Absent[I, Decoder, i] @unchecked =>
            Decoder.instance(_ => Option.empty[i].asRight[DecodingFailure])
        }
      }
    )

  given [P[_]: FromJson, Q[_]: FromJson]: FromJson[EitherK[P, Q, _]] =
    new FromJson[EitherK[P, Q, _]] {
      override val decoder: EitherK[P, Q, _] ~> Decoder = new (EitherK[P, Q, _] ~> Decoder) {
        override def apply[A](p: EitherK[P, Q, A]): Decoder[A] =
          p.run.fold(
            FromJson[P].decoder(_),
            FromJson[Q].decoder(_),
          )
      }
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[FromJson]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: FromJson[S]): FromJson[S] = instance

  object ops {
    implicit def toAllFromJsonOps[S[_], A](target: S[A])(implicit tc: FromJson[S]): AllOps[S, A] {
      type TypeClassType = FromJson[S]
    } = new AllOps[S, A] {
      type TypeClassType = FromJson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: FromJson[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToFromJsonOps extends Serializable {
    implicit def toFromJsonOps[S[_], A](target: S[A])(implicit tc: FromJson[S]): Ops[S, A] {
      type TypeClassType = FromJson[S]
    } = new Ops[S, A] {
      type TypeClassType = FromJson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToFromJsonOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
