package morphling.circe

import cats._
import cats.data.Const
import cats.free._
import cats.instances.function._
import cats.syntax.all._
import io.circe.Json
import morphling._
import morphling.HFunctor._
import morphling.Schema.Schema
import mouse.option._
import simulacrum.typeclass

/**
  * Allows to filter Json via specific schema
  */
@typeclass
trait ToFilter[S[_]] {
  def filter: S ~> Const[Endo[Json], *]
}

object ToFilter {
  type JsonFilter[T] = Const[Endo[Json], T]

  implicit class ToFilterOps[S[_], A](s: S[A]) {
    def jsonFilter(implicit TF: ToFilter[S]): Endo[Json] = TF.filter(s).getConst
  }

  implicit def schemaToFilter[P[_]: ToFilter]: ToFilter[Schema[P, *]] = new ToFilter[Schema[P, *]] {
    val filter: Schema[P, *] ~> JsonFilter = new (Schema[P, *] ~> JsonFilter) {
      override def apply[I](schema: Schema[P, I]): JsonFilter[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], JsonFilter](filterAlg[P]).apply(schema)
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
                case Alt(id, f, p) =>
                  extractField(id) |+| f.getConst
              }.fold,
            s.alts.map {
              case Alt(id, f, p) =>
                extractFieldContents(id, f.getConst)
            }.fold
          )
        }
        case s: RecordSchema[P, JsonFilter, I] => recordFilter[P,I](s.props)
        case s: IsoSchema[P, JsonFilter, i0, I] => s.base.retag[I]
      }
    }

  def recordFilter[P[_]: ToFilter, I](rb: FreeApplicative[PropSchema[I, JsonFilter, *], I]): JsonFilter[I] = {
    rb.foldMap[JsonFilter](
      new (PropSchema[I, JsonFilter, *] ~> JsonFilter) {
        override def apply[B](ps: PropSchema[I, JsonFilter, B]): JsonFilter[B] = {
          ps match {
            case req: Required[I, JsonFilter, i] => Const.of(extractField(req.fieldName))
            case opt: Optional[I, JsonFilter, i] => Const.of(extractField(opt.fieldName))
            case _ => Const.of(ejm.empty)
          }
        }
      }
    )
  }

  private def extractField(name: String): Endo[Json] = { j =>
    j.mapObject(_.filterKeys(_ == name))
  }

  private def extractFieldContents(name: String, inner: Endo[Json]): Endo[Json] = { j =>
    j.mapObject(_.filterKeys(_ == name).mapValues(inner))
  }

  private implicit val ejm: Monoid[Endo[Json]] = new Monoid[Endo[Json]] {
    override def empty: Endo[Json] = _ => Json.Null

    override def combine(x: Endo[Json], y: Endo[Json]): Endo[Json] =
      x &&& y andThen { case (lhs, rhs) => lhs deepMerge rhs}
  }
}
