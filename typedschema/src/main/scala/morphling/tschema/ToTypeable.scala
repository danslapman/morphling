package morphling.tschema

import cats._
import cats.free._
import cats.syntax.option._
import morphling.HFunctor._
import morphling._
import morphling.Schema.Schema
import ru.tinkoff.tschema.swagger.{SwaggerOneOf, SwaggerTypeable}
import simulacrum.typeclass

@typeclass
trait ToTypeable[S[_]] {
  def toTypeable: S ~> SwaggerTypeable
}

object ToTypeable {
  implicit class ToGenOps[S[_], A](s: S[A]) {
    def toTypeable(implicit TT: ToTypeable[S]): SwaggerTypeable[A] = TT.toTypeable(s)
  }

  implicit def schemaToTypeable[P[_]: ToTypeable]: ToTypeable[Schema[P, ?]] = new ToTypeable[Schema[P, ?]] {
    def toTypeable = new (Schema[P, ?] ~> SwaggerTypeable) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], SwaggerTypeable](typAlg).apply(schema)
      }
    }
  }

  def typAlg[P[_]: ToTypeable]: HAlgebra[SchemaF[P, ?[_], ?], SwaggerTypeable] =
    new HAlgebra[SchemaF[P, ?[_], ?], SwaggerTypeable] {
      def apply[I](schema: SchemaF[P, SwaggerTypeable, I]): SwaggerTypeable[I] = schema match {
        case s: PrimSchema[P, SwaggerTypeable, I] => ToTypeable[P].toTypeable(s.prim)
        case s: OneOfSchema[P, SwaggerTypeable, I] =>
          SwaggerTypeable.make(
            SwaggerOneOf(
              s.alts.map {
                case Alt(field, b, p) =>
                  field.some -> Eval.now(b.typ)
              }.toList.toVector
            )
          )

        case s: RecordSchema[P, SwaggerTypeable, I] => recordTypeable[P,I](s.props)
        case s: IsoSchema[P, SwaggerTypeable, i0, I] => s.base.as[I]
      }
    }

  def recordTypeable[P[_]: ToTypeable, I](rb: FreeApplicative[PropSchema[I, SwaggerTypeable, ?], I]): SwaggerTypeable[I] = {
    implicit val stap: Applicative[SwaggerTypeable] = new Applicative[SwaggerTypeable] {
      override def pure[T](x: T): SwaggerTypeable[T] = SwaggerTypeable.swaggerTypeableUnit.as[T]

      override def ap[T, U](ff: SwaggerTypeable[T => U])(fa: SwaggerTypeable[T]): SwaggerTypeable[U] = {
        fa.as[U]
      }
    }

    rb.foldMap(
      new (PropSchema[I, SwaggerTypeable, ?] ~> SwaggerTypeable) {
        def apply[B](ps: PropSchema[I, SwaggerTypeable, B]): SwaggerTypeable[B] = ps match {
          case Required(_, base, _, _) => base
          case opt: Optional[I, SwaggerTypeable, i] => SwaggerTypeable.optionTypeable(opt.base)
        }
      }
    )
  }
}