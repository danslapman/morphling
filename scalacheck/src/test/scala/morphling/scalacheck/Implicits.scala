package morphling.scalacheck

import cats._
import morphling.Schema.Schema
import morphling.protocol._
import morphling.protocol.SType.SSchema

object Implicits {
  implicit val toGen: ToGen[SSchema] = new ToGen[SSchema] { self =>
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen
    import org.scalacheck.Gen._
    val toGen = new (SSchema ~> Gen) {
      def apply[A](s: SSchema[A]): Gen[A] = s.unmutu match {
        case SNullT()   => arbitrary[Unit]
        case SBoolT()   => arbitrary[Boolean]
        case SIntT()    => arbitrary[Int]
        case SLongT()   => Gen.chooseNum(Long.MinValue + 808L, Long.MaxValue) // Magic number to circumvent Instant#toEpochMillis throwing exceptions
        case SFloatT()  => arbitrary[Float]
        case SDoubleT() => arbitrary[Double]
        case SCharT()   => arbitrary[Char]
        case SStrT()    => arbitrary[String]
        case arr: SArrayT[Schema[SSchema, *], i] =>
          val baseDecoder: Gen[i] = ToGen.schemaToGen[SSchema](self).toGen(arr.elem)
          containerOf[Vector, i](baseDecoder)
      }
    }
  }
}
