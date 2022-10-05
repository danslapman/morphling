package morphling.circe

import scala.annotation.implicitNotFound

import cats.*
import cats.data.EitherK
import cats.data.State
import cats.data.State.*
import cats.free.*
import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import morphling.*
import morphling.Schema.*
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import simulacrum.typeclass

@implicitNotFound("Could not find an instance of ToJson for ${S}")
@typeclass
trait ToJson[S[_]] extends Serializable {
  def encoder: S ~> Encoder
}

object ToJson {
  implicit class ToJsonOps[F[_], A](private val fa: F[A]) {
    def encoder(implicit TJ: ToJson[F]): Encoder[A] = TJ.encoder(fa)
  }

  implicit def schemaToJson[P[_]: ToJson]: ToJson[Schema[P, *]] = new ToJson[Schema[P, *]] {
    override val encoder: Schema[P, *] ~> Encoder = new (Schema[P, *] ~> Encoder) {
      override def apply[I](schema: Schema[P, I]): Encoder[I] =
        HFix.cataNT[SchemaF[P, *[_], *], Encoder](serializeAlg).apply(schema)
    }
  }

  implicit def annSchemaToJson[P[_]: ToJson, A[_]]: ToJson[AnnotatedSchema[P, A, *]] =
    new ToJson[AnnotatedSchema[P, A, *]] {
      override val encoder: AnnotatedSchema[P, A, *] ~> Encoder = new (AnnotatedSchema[P, A, *] ~> Encoder) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): Encoder[I] =
          HFix
            .cataNT[SchemaF[P, *[_], *], Encoder](serializeAlg)
            .apply(
              HFix.forget[SchemaF[P, *[_], *], A].apply(schema)
            )
      }
    }

  def serializeAlg[P[_]: ToJson]: HAlgebra[SchemaF[P, *[_], *], Encoder] =
    new HAlgebra[SchemaF[P, *[_], *], Encoder] {
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

  def serializeObjF[P[_]: ToJson, I](rb: FreeApplicative[PropSchema[I, Encoder, *], I]): Encoder[I] = { (value: I) =>
    Json.fromJsonObject(
      rb.foldMap[State[JsonObject, *]](
        new (PropSchema[I, Encoder, *] ~> State[JsonObject, *]) {
          def apply[B](ps: PropSchema[I, Encoder, B]): State[JsonObject, B] =
            for {
              _ <- modify { (obj: JsonObject) =>
                ps match {
                  case req: Required[I, Encoder, i] =>
                    (req.fieldName, req.base(req.extract.extract(value))) +: obj

                  case opt: Optional[I, Encoder, i] =>
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

  implicit def eitherKToJson[P[_]: ToJson, Q[_]: ToJson]: ToJson[EitherK[P, Q, *]] =
    new ToJson[EitherK[P, Q, *]] {
      override val encoder = new (EitherK[P, Q, *] ~> Encoder) {
        def apply[A](p: EitherK[P, Q, A]): Encoder[A] =
          p.run.fold(ToJson[P].encoder(_), ToJson[Q].encoder(_))
      }
    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ToJson]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: ToJson[S]): ToJson[S] = instance

  object ops {
    implicit def toAllToJsonOps[S[_], A](target: S[A])(implicit tc: ToJson[S]): AllOps[S, A] {
      type TypeClassType = ToJson[S]
    } = new AllOps[S, A] {
      type TypeClassType = ToJson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: ToJson[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToToJsonOps extends Serializable {
    implicit def toToJsonOps[S[_], A](target: S[A])(implicit tc: ToJson[S]): Ops[S, A] {
      type TypeClassType = ToJson[S]
    } = new Ops[S, A] {
      type TypeClassType = ToJson[S]
      val self: S[A]                       = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToToJsonOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
