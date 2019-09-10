package morphling.scalacheck.annotated

import cats.{Endo, ~>}
import morphling.protocol.annotated.{Non, Range, Restriction}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.scalacheck.{GenPack, ToGen}
import org.scalacheck.Gen

object Implicits extends GenPack {
  implicit val genRestriction: (Restriction ~> λ[T => Endo[Gen[T]]]) =
    new (Restriction ~> λ[T => Endo[Gen[T]]]) {
      override def apply[A](rs: Restriction[A]): Endo[Gen[A]] = rs match {
        case Non => identity
        case Range(from, to) =>
          (gen: Gen[Int]) => gen.filter(i => i > from && i < to)
      }
    }

  implicit val primToGen: ToGen[ASchema] = new ToGen[ASchema] { self =>
    val toGen = new (ASchema ~> Gen) {
      def apply[I](s: ASchema[I]): Gen[I] = sTypeGen[ASchema[I]#Inner].apply(s.unmutu)
    }
  }
}
