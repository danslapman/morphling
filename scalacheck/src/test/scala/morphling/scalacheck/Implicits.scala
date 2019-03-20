package morphling.scalacheck

import cats._
import morphling.Schema.Schema
import morphling.json._
import morphling.json.JType.JSchema

object Implicits {
  implicit val toGen: ToGen[JSchema] = new ToGen[JSchema] { self =>
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen
    import org.scalacheck.Gen._
    def toGen = new (JSchema ~> Gen) {
      def apply[A](s: JSchema[A]): Gen[A] = s.unmutu match {
        case JNullT()   => arbitrary[Unit]
        case JBoolT()   => arbitrary[Boolean]
        case JByteT()   => arbitrary[Byte]
        case JShortT()  => arbitrary[Short]
        case JIntT()    => arbitrary[Int]
        case JLongT()   => Gen.chooseNum(Long.MinValue + 808L, Long.MaxValue) // Magic number to circumvent Instant#toEpochMillis throwing exceptions
        case JFloatT()  => arbitrary[Float]
        case JDoubleT() => arbitrary[Double]
        case JCharT()   => arbitrary[Char]
        case JStrT()    => arbitrary[String]
        case arr: JArrayT[Schema[JSchema, ?], i] =>
          val baseDecoder: Gen[i] = ToGen.schemaToGen[JSchema](self).toGen(arr.elem)
          containerOf[Vector, i](baseDecoder)
      }
    }
  }
}
