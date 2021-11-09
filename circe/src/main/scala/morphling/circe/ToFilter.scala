package morphling.circe

import cats.*
import cats.data.{Const, EitherK}
import cats.free.*
import cats.instances.function.*
import cats.instances.option.*
import cats.syntax.all.*
import io.circe.{Json, JsonObject}
import morphling.*
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option.*
import simulacrum.typeclass

/**
  * Allows to filter Json via specific schema
  */
@typeclass
trait ToFilter[S[_]] {
  def filter: S ~> Const[Json => Option[Json], *]
}

object ToFilter {
  type Subset[T] = T => Option[T]
  type JsonFilter[T] = Const[Subset[Json], T]

  implicit class ToFilterOps[S[_], A](s: S[A]) {
    def jsonFilter(implicit TF: ToFilter[S]): Subset[Json] = TF.filter(s).getConst
  }

  implicit def schemaToFilter[P[_]: ToFilter]: ToFilter[Schema[P, *]] = new ToFilter[Schema[P, *]] {
    override val filter: Schema[P, *] ~> JsonFilter = new (Schema[P, *] ~> JsonFilter) {
      override def apply[I](schema: Schema[P, I]): JsonFilter[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], JsonFilter](filterAlg[P]).apply(schema)
      }
    }
  }

  implicit def annSchemaToFilter[P[_]: ToFilter, A[_]: *[_] ~> λ[T => Endo[JsonFilter[T]]]]: ToFilter[AnnotatedSchema[P, A, *]] =
    new ToFilter[AnnotatedSchema[P, A, *]] {
      override val filter: AnnotatedSchema[P, A, *] ~> JsonFilter = new (AnnotatedSchema[P, A, *] ~> JsonFilter) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): JsonFilter[I] = {
          HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], JsonFilter](annFilterAlg).apply(schema)
        }
      }
    }

  def filterAlg[P[_]: ToFilter]: HAlgebra[SchemaF[P, *[_], *], JsonFilter] =
    new HAlgebra[SchemaF[P, *[_], *], JsonFilter] {
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
        case s: RecordSchema[P, JsonFilter, I] => recordFilter[P,I](s.props)
        case s: IsoSchema[P, JsonFilter, i0, I] => s.base.retag[I]
      }
    }

  def annFilterAlg[P[_]: ToFilter, Ann[_]](implicit interpret: Ann ~> λ[T => Endo[JsonFilter[T]]]): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], JsonFilter] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], JsonFilter] {
      override def apply[A](schema: HEnvT[Ann, SchemaF[P, *[_], *], JsonFilter, A]): JsonFilter[A] =
        interpret.apply(schema.ask).apply(filterAlg[P].apply(schema.fa))
    }

  def recordFilter[P[_]: ToFilter, I](rb: FreeApplicative[PropSchema[I, JsonFilter, *], I]): JsonFilter[I] = {
    rb.foldMap[JsonFilter](
      new (PropSchema[I, JsonFilter, *] ~> JsonFilter) {
        override def apply[B](ps: PropSchema[I, JsonFilter, B]): JsonFilter[B] = {
          ps match {
            case req: Required[I, JsonFilter, i] => Const.of(extractFieldContentsStrict(req.fieldName, req.base.getConst))
            case opt: Optional[I, JsonFilter, i] => Const.of(extractFieldContents(opt.fieldName, opt.base.getConst))
            case _ => Const.of(sjm.empty)
          }
        }
      }
    )
  }

  implicit def eitherKToFilter[P[_]: ToFilter, Q[_]: ToFilter]: ToFilter[EitherK[P, Q, *]] =
    new ToFilter[EitherK[P, Q, *]] {
      override val filter = new (EitherK[P, Q, *] ~> JsonFilter) {
        def apply[A](p: EitherK[P, Q, A]): JsonFilter[A] = {
          p.run.fold(ToFilter[P].filter(_), ToFilter[Q].filter(_))
        }
      }
    }

  private def extractField(name: String): Subset[Json] = { j =>
    j.mapObject(_.filterKeys(_ == name)).asObject.filter(_.nonEmpty).map(Json.fromJsonObject)
  }

  private def extractFieldContents(name: String, inner: Subset[Json]): Subset[Json] = { j =>
    j.mapObject(jo => JsonObject.fromIterable(jo.filterKeys(_ == name).toIterable.flatMap { case (k, v) => inner(v).map(k -> _)}))
      .asObject.map(Json.fromJsonObject)
  }

  private def extractFieldContentsStrict(name: String, inner: Subset[Json]): Subset[Json] = { j =>
    j.mapObject(jo => JsonObject.fromIterable(jo.filterKeys(_ == name).toIterable.flatMap { case (k, v) => inner(v).map(k -> _)}))
      .asObject.filter(_.nonEmpty).map(Json.fromJsonObject)
  }

  private implicit val semiJ: Semigroup[Json] = _ deepMerge _

  private implicit val sjm: Monoid[Subset[Json]] = new Monoid[Subset[Json]] {
    override val empty: Subset[Json] = _ => None

    override def combine(x: Subset[Json], y: Subset[Json]): Subset[Json] =
      x &&& y andThen { case (lhs, rhs) => lhs |+| rhs}
  }
}
