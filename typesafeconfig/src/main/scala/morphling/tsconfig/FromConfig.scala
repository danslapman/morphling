package morphling.tsconfig

import cats._
import cats.data.EitherK
import cats.free._
import cats.instances.function._
import com.typesafe.config.{Config, ConfigException}
import morphling.HFunctor.HAlgebra
import morphling.Schema.Schema
import morphling.{Alt, HFix, IsoSchema, OneOfSchema, Optional, PrimSchema, PropSchema, RecordSchema, Required, SchemaF}
import mouse.any._
import mouse.boolean._
import simulacrum.typeclass

import scala.collection.JavaConversions._

@typeclass
trait FromConfig[S[_]] {
  def reader: S ~> (Config => ?)
}

object FromConfig {
  implicit class FromConfigOps[F[_], A](fa: F[A]) {
    def fromJson(a: Config)(implicit FC: FromConfig[F]): A = {
      FC.reader(fa)(a)
    }
  }

  implicit def schemaFromConfig[P[_]: FromConfig]: FromConfig[Schema[P, ?]] = new FromConfig[Schema[P, ?]] {
    def reader = new (Schema[P, ?] ~> (Config => ?)) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], Config => ?](readerAlg[P]).apply(schema)
      }
    }
  }

  def readerAlg[P[_]: FromConfig]: HAlgebra[SchemaF[P, ?[_], ?], Config => ?] =
    new HAlgebra[SchemaF[P, ?[_], ?], Config => ?] {
      def apply[I](s: SchemaF[P, Config => ?, I]): Config => I = s match {
        case PrimSchema(p) => FromConfig[P].reader(p)

        case OneOfSchema(alts) =>
          (c: Config) =>
            val results = for {
              fields <- c.entrySet().toList.map(_.getKey)
              altResult <- alts.toList flatMap {
                case Alt(id, base, prism) =>
                  fields.contains(id).option(
                    c.atKey(id) |> base |> prism.reverseGet
                  ).toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => throw new ConfigException.Missing(altIds.toList.mkString(", "))
              case _ => throw new ConfigException.BadValue(altIds.toList.mkString(", "), "More than one matching field found")
            }

        case RecordSchema(rb) =>
          readObj(rb)

        case IsoSchema(base, iso) =>
          base andThen iso.get
      }
    }

  def readObj[I](rb: FreeApplicative[PropSchema[I, Config => ?, ?], I]): Config => I = {
    rb.foldMap(
      new (PropSchema[I, Config => ?, ?] ~> (Config => ?)) {
        def apply[B](ps: PropSchema[I, Config => ?, B]): Config => B = ps match {
          case Required(field, base, _, _) =>
            _.atKey(field) |> base

          case opt: Optional[I, Config => ?, i] =>
            conf => conf.hasPath(opt.fieldName).fold(
              Some(conf.atKey(opt.fieldName) |> opt.base),
              None
            )
        }
      }
    )
  }

  implicit def eitherKFromConfig[P[_]: FromConfig, Q[_]: FromConfig] = new FromConfig[EitherK[P, Q, ?]] {
    val reader = new (EitherK[P, Q, ?] ~> (Config => ?)) {
      def apply[A](p: EitherK[P, Q, A]): Config => A = {
        p.run.fold(
          FromConfig[P].reader(_),
          FromConfig[Q].reader(_),
        )
      }
    }
  }
}
