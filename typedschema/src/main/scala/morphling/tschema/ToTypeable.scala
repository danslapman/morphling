package morphling.tschema

import cats._
import cats.free._
import cats.data.Const
import cats.data.Const._
import morphling.HFunctor._
import morphling._
import morphling.Schema.Schema
import mouse.option._
import ru.tinkoff.tschema.swagger.{SwaggerObject, SwaggerOneOf, SwaggerPrimitive, SwaggerProperty, SwaggerTypeable}
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
    val toTypeable: Schema[P, ?] ~> SwaggerTypeable = new (Schema[P, ?] ~> SwaggerTypeable) {
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
              s.discriminator.cata(
                dField => {
                  val discriminatorProp = SwaggerProperty(dField, None, Eval.now(SwaggerPrimitive.string))
                  s.alts.map {
                    case Alt(field, b, p) =>
                      Option.empty[String] -> Eval.now(b.typ match {
                        case SwaggerObject(properties, required) =>
                          SwaggerObject(properties :+ discriminatorProp, required.map(_ :+ dField))
                        case other => other
                      })
                    }.toList.toVector
                },
                s.alts.map {
                  case Alt(field, b, p) =>
                    Option.empty[String] -> Eval.now(SwaggerObject(Vector(
                      SwaggerProperty(field, None, Eval.now(b.typ))
                    ), Eval.now(Vector(field))))
                }.toList.toVector
              ),
              s.discriminator
            )
          )

        case s: RecordSchema[P, SwaggerTypeable, I] => recordTypeable[P,I](s.props)
        case s: IsoSchema[P, SwaggerTypeable, i0, I] => s.base.as[I]
      }
    }

  def recordTypeable[P[_]: ToTypeable, I](rb: FreeApplicative[PropSchema[I, SwaggerTypeable, ?], I]): SwaggerTypeable[I] = {
    implicit val som: Monoid[SwaggerObject] = new Monoid[SwaggerObject] {
      override def empty: SwaggerObject = SwaggerObject()

      override def combine(x: SwaggerObject, y: SwaggerObject): SwaggerObject =
        SwaggerObject(
          (x.properties ++ y.properties).groupBy(_.name).map(_._2.head).toVector, //.distinctBy(_.name)
          (for {
            xr <- x.required
            yr <- y.required
          } yield (xr ++ yr).distinct).memoize
        )
    }

    SwaggerTypeable.make[I](
      rb.foldMap[Const[SwaggerObject, ?]](
        new (PropSchema[I, SwaggerTypeable, ?] ~> Const[SwaggerObject, ?]) {
          def apply[B](ps: PropSchema[I, SwaggerTypeable, B]): Const[SwaggerObject, B] = {
            ps match {
              case req: Required[I, SwaggerTypeable, i] =>
                req.default.cata(
                  default => {
                    val optionalField = SwaggerProperty(req.fieldName, None, Eval.now(req.base.typ))
                    Const.of(SwaggerObject(Vector(optionalField)))
                  },
                  {
                    val requiredField = SwaggerProperty(req.fieldName, None, Eval.now(req.base.typ))
                    Const.of(SwaggerObject(Vector(requiredField), Eval.now(Vector(req.fieldName))))
                  }
                )

              case opt: Optional[I, SwaggerTypeable, i] =>
                val optionalField = SwaggerProperty(opt.fieldName, None, Eval.now(opt.base.typ))
                Const.of(SwaggerObject(Vector(optionalField)))
            }
          }
        }
      ).getConst
    )
  }
}