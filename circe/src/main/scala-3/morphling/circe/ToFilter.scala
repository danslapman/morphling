package morphling.circe

import cats.*
import cats.data.{Const, EitherK}
import cats.free.*
import cats.instances.function.*
import cats.instances.option.*
import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import morphling.*
import morphling.given
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
  * Allows to filter Json via specific schema
  */
@implicitNotFound("Could not find an instance of ToFilter for ${S}")
@typeclass
trait ToFilter[S[_]] extends Serializable {
  def filter: S ~> Const[Json => Option[Json], *]

  extension[S[_], A](s: S[A])(using TF: ToFilter[S])
    def jsonFilter: ToFilter.Subset[Json] = TF.filter(s).getConst
}

object ToFilter {
  type Subset[T] = T => Option[T]
  type JsonFilter[T] = Const[Subset[Json], T]

  given [P[_]: ToFilter]: ToFilter[Schema[P, _]] =
    new ToFilter[Schema[P, _]] {
      override val filter: Schema[P, _] ~> JsonFilter = new (Schema[P, _] ~> JsonFilter) {
        override def apply[I](schema: Schema[P, I]): JsonFilter[I] = {
          HFix.cataNT[[Y[_], Z] =>> SchemaF[P, Y, Z], JsonFilter](filterAlg[P]).apply(schema)
        }
      }
    }

  given [P[_]: ToFilter, A[_]: [Y[_]] =>> Y ~> ([T] =>> Endo[JsonFilter[T]])]: ToFilter[AnnotatedSchema[P, A, *]] =
    new ToFilter[AnnotatedSchema[P, A, _]] {
      override val filter: AnnotatedSchema[P, A, _] ~> JsonFilter = new (AnnotatedSchema[P, A, _] ~> JsonFilter) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): JsonFilter[I] = {
          HFix.cataNT[[Y1[_], Z1] =>> HEnvT[A, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], JsonFilter](annFilterAlg).apply(schema)
        }
      }
    }

  def filterAlg[P[_] : ToFilter]: HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], JsonFilter] =
    new HAlgebra[[Y[_], Z] =>> SchemaF[P, Y, Z], JsonFilter] {
      override def apply[I](schema: SchemaF[P, JsonFilter, I]): JsonFilter[I] = schema match {
        case s: PrimSchema[P, JsonFilter, I] => ToFilter[P].filter(s.prim)
        case s: OneOfSchema[P, JsonFilter, I] => Const.of {
          s.discriminator.cata(
            dField =>
              s.alts.map {
                case Alt(_, f, _) =>
                  extractField(dField) |+| f.getConst
              }.fold,
            s.alts.map {
              case Alt(id, f, _) =>
                extractFieldContentsStrict(id, f.getConst)
            }.fold
          )
        }
        case s: RecordSchema[P, JsonFilter, I] => recordFilter[P, I](s.props)
        case s: IsoSchema[P, JsonFilter, i0, I] => s.base.retag[I]
      }
    }

  def annFilterAlg[P[_] : ToFilter, Ann[_]](implicit interpret: Ann ~> ([T] =>> Endo[JsonFilter[T]])): HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], JsonFilter] =
    new HAlgebra[[Y1[_], Z1] =>> HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], Y1, Z1], JsonFilter] {
      override def apply[A](schema: HEnvT[Ann, [Y[_], Z] =>> SchemaF[P, Y, Z], JsonFilter, A]): JsonFilter[A] =
        interpret.apply(schema.ask).apply(filterAlg[P].apply(schema.fa))
    }

  def recordFilter[P[_] : ToFilter, I](rb: FreeApplicative[PropSchema[I, JsonFilter, _], I]): JsonFilter[I] = {
    rb.foldMap[JsonFilter](
      new (PropSchema[I, JsonFilter, _] ~> JsonFilter) {
        override def apply[B](ps: PropSchema[I, JsonFilter, B]): JsonFilter[B] = {
          ps match {
            case req: Required[I, JsonFilter, i] => Const.of(extractFieldContentsStrict(req.fieldName, req.base.getConst))
            case opt: Optional[I, JsonFilter, i] @unchecked => Const.of(extractFieldContents(opt.fieldName, opt.base.getConst))
            case _ => Const.of(Monoid[Subset[Json]].empty)
          }
        }
      }
    )
  }

  given[P[_]: ToFilter, Q[_]: ToFilter]: ToFilter[EitherK[P, Q, _]] =
    new ToFilter[EitherK[P, Q, _]] {
      override val filter = new (EitherK[P, Q, _] ~> JsonFilter) {
        def apply[A](p: EitherK[P, Q, A]): JsonFilter[A] = {
          p.run.fold(ToFilter[P].filter(_), ToFilter[Q].filter(_))
        }
      }
    }

  private def extractField(name: String): Subset[Json] = { j =>
    j.mapObject(_.filterKeys(_ == name)).asObject.filter(_.nonEmpty).map(Json.fromJsonObject)
  }

  private def extractFieldContents(name: String, inner: Subset[Json]): Subset[Json] = { j =>
    j.mapObject(jo => JsonObject.fromIterable(jo.filterKeys(_ == name).toIterable.flatMap { case (k, v) => inner(v).map(k -> _) }))
      .asObject.map(Json.fromJsonObject)
  }

  private def extractFieldContentsStrict(name: String, inner: Subset[Json]): Subset[Json] = { j =>
    j.mapObject(jo => JsonObject.fromIterable(jo.filterKeys(_ == name).toIterable.flatMap { case (k, v) => inner(v).map(k -> _) }))
      .asObject.filter(_.nonEmpty).map(Json.fromJsonObject)
  }

  private given Semigroup[Json] = _ deepMerge _

  private given Monoid[Subset[Json]] = new Monoid[Subset[Json]] {
    override val empty: Subset[Json] = _ => None

    override def combine(x: Subset[Json], y: Subset[Json]): Subset[Json] =
      x &&& y andThen { case (lhs, rhs) => lhs |+| rhs }
  }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[ToFilter]] for `S`.
   */
  @inline def apply[S[_]](implicit instance: ToFilter[S]): ToFilter[S] = instance

  object ops {
    implicit def toAllToFilterOps[S[_], A](target: S[A])(implicit tc: ToFilter[S]): AllOps[S, A] {
      type TypeClassType = ToFilter[S]
    } = new AllOps[S, A] {
      type TypeClassType = ToFilter[S]
      val self: S[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[S[_], A] extends Serializable {
    type TypeClassType <: ToFilter[S]
    def self: S[A]
    val typeClassInstance: TypeClassType
  }
  trait AllOps[S[_], A] extends Ops[S, A]
  trait ToToFilterOps extends Serializable {
    implicit def toToFilterOps[S[_], A](target: S[A])(implicit tc: ToFilter[S]): Ops[S, A] {
      type TypeClassType = ToFilter[S]
    } = new Ops[S, A] {
      type TypeClassType = ToFilter[S]
      val self: S[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  object nonInheritedOps extends ToToFilterOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */


}