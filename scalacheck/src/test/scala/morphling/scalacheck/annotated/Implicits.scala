package morphling.scalacheck.annotated

import cats.arrow.FunctionK
import cats.~>
import morphling.annotated.AnnotationProcessor
import morphling.protocol.annotated.{NoRestr, Range, Restriction}
import morphling.protocol.{SArrayT, SBoolT, SCharT, SDoubleT, SFloatT, SIntT, SLongT, SNullT, SStrT}
import morphling.protocol.annotated.STypeAnn.ASchema
import morphling.scalacheck.ToGen
import org.scalacheck.Gen

object Implicits {
  implicit val proc: AnnotationProcessor[Restriction, Gen] =
    new AnnotationProcessor[Restriction, Gen] {
      override def process: Restriction => Gen ~> Gen = {
        case NoRestr => FunctionK.id
        case Range(from, to) => FunctionK.id
      }
    }

  implicit val toGen: ToGen[ASchema] = new ToGen[ASchema] { self =>
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen._

    val toGen = new (ASchema ~> Gen) {
      def apply[A](s: ASchema[A]): Gen[A] = s.unmutu match {
        case SNullT()   => arbitrary[Unit]
        case SBoolT()   => arbitrary[Boolean]
        case SIntT()    => arbitrary[Int]
        case SLongT()   => Gen.chooseNum(Long.MinValue + 808L, Long.MaxValue) // Magic number to circumvent Instant#toEpochMillis throwing exceptions
        case SFloatT()  => arbitrary[Float]
        case SDoubleT() => arbitrary[Double]
        case SCharT()   => arbitrary[Char]
        case SStrT()    => arbitrary[String]
        case arr: SArrayT[s.Inner, i] =>
          val elemGen: Gen[i] = ToGen.annSchemaToGen[ASchema, Restriction](self, proc).toGen(arr.elem)
          containerOf[Vector, i](elemGen)
      }
    }
  }
}
