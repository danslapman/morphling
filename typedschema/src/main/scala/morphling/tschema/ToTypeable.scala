package morphling.tschema

import cats._
import cats.data.{Const, EitherK}
import cats.data.Const._
import cats.free._
import cats.syntax.option._
import morphling._
import morphling.HFunctor._
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import mouse.option._
import ru.tinkoff.tschema.swagger.{SwaggerObject, SwaggerOneOf, SwaggerPrimitive, SwaggerProperty, SwaggerRef, SwaggerTypeable}
import simulacrum.typeclass

@typeclass
trait ToTypeable[S[_]] {
  def toTypeable: S ~> SwaggerTypeable
}

object ToTypeable {
  implicit class ToTypeableOps[S[_], A](s: S[A]) {
    def typeable(implicit TT: ToTypeable[S]): SwaggerTypeable[A] = TT.toTypeable(s)
  }

  implicit def schemaToTypeable[P[_]: ToTypeable]: ToTypeable[Schema[P, *]] = new ToTypeable[Schema[P, *]] {
    override val toTypeable: Schema[P, *] ~> SwaggerTypeable = new (Schema[P, *] ~> SwaggerTypeable) {
      override def apply[I](schema: Schema[P, I]): SwaggerTypeable[I] = {
        HFix.cataNT[SchemaF[P, *[_], *], SwaggerTypeable](typAlg[P]).apply(schema)
      }
    }
  }

  implicit def annSchemaToTypeable[P[_]: ToTypeable, A[_]: *[_] ~> λ[T => Endo[SwaggerTypeable[T]]]]: ToTypeable[AnnotatedSchema[P, A, *]] =
    new ToTypeable[AnnotatedSchema[P, A, *]] {
      override val toTypeable: AnnotatedSchema[P, A, *] ~> SwaggerTypeable = new (AnnotatedSchema[P, A, *] ~> SwaggerTypeable) {
        override def apply[I](schema: AnnotatedSchema[P, A, I]): SwaggerTypeable[I] = {
          HFix.cataNT[HEnvT[A, SchemaF[P, *[_], *], *[_], *], SwaggerTypeable](annTypAlg).apply(schema)
        }
      }
    }

  def typAlg[P[_]: ToTypeable]: HAlgebra[SchemaF[P, *[_], *], SwaggerTypeable] =
    new HAlgebra[SchemaF[P, *[_], *], SwaggerTypeable] {
      def apply[I](schema: SchemaF[P, SwaggerTypeable, I]): SwaggerTypeable[I] = schema match {
        case s: PrimSchema[P, SwaggerTypeable, I] => ToTypeable[P].toTypeable(s.prim)
        case s: OneOfSchema[P, SwaggerTypeable, I] =>
          SwaggerTypeable.make(
            SwaggerOneOf(
              s.discriminator.cata(
                dField => {
                  def discriminatorProp(id: String) =
                    SwaggerProperty(dField, None, Eval.now(SwaggerPrimitive.string.mod(_.copy(pattern = id.some))))

                  s.alts.map {
                    case Alt(id, b, p) =>
                      Option(id) -> Eval.now(b.typ match {
                        case SwaggerObject(properties, required, discriminator) =>
                          SwaggerRef(
                            id, None, Eval.now(
                              SwaggerObject(properties :+ discriminatorProp(id), required.map(_ :+ dField), discriminator)
                            )
                          )
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

  def annTypAlg[P[_]: ToTypeable, Ann[_]](implicit interpret: Ann ~> λ[T => Endo[SwaggerTypeable[T]]]): HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], SwaggerTypeable] =
    new HAlgebra[HEnvT[Ann, SchemaF[P, *[_], *], *[_], *], SwaggerTypeable] {
      override def apply[A](schema: HEnvT[Ann, SchemaF[P, *[_], *], SwaggerTypeable, A]): SwaggerTypeable[A] =
        interpret.apply(schema.ask).apply(typAlg[P].apply(schema.fa))
    }

  def recordTypeable[P[_]: ToTypeable, I](rb: FreeApplicative[PropSchema[I, SwaggerTypeable, *], I]): SwaggerTypeable[I] = {
    implicit val som: Monoid[SwaggerObject] = new Monoid[SwaggerObject] {
      override val empty: SwaggerObject = SwaggerObject()

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
      rb.foldMap[Const[SwaggerObject, *]](
        new (PropSchema[I, SwaggerTypeable, *] ~> Const[SwaggerObject, *]) {
          def apply[B](ps: PropSchema[I, SwaggerTypeable, B]): Const[SwaggerObject, B] = {
            ps match {
              case req: Required[I, SwaggerTypeable, i] =>
                req.default.cata(
                  _ => {
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
              case const: Constant[I, SwaggerTypeable, i] =>
                Const.of(SwaggerObject.withProps())
              case abs: Absent[I, SwaggerTypeable, i] =>
                Const.of(SwaggerObject())
            }
          }
        }
      ).getConst
    )
  }

  implicit def eitherKTypeable[P[_]: ToTypeable, Q[_]: ToTypeable]: ToTypeable[EitherK[P, Q, *]] =
    new ToTypeable[EitherK[P, Q, *]] {
      override val toTypeable: EitherK[P, Q, *] ~> SwaggerTypeable =
        new (EitherK[P, Q, *] ~> SwaggerTypeable) {
          override def apply[A](fa: EitherK[P, Q, A]): SwaggerTypeable[A] = fa.run.fold(
            ToTypeable[P].toTypeable(_),
            ToTypeable[Q].toTypeable(_),
          )
        }
    }
}