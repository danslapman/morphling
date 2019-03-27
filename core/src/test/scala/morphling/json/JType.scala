package morphling.json

import morphling.HMutu
import morphling.Schema._

sealed trait JType[F[_], I]

case class JNullT[F[_]]()   extends JType[F, Unit]
case class JBoolT[F[_]]()   extends JType[F, Boolean]

case class JIntT[F[_]]()    extends JType[F, Int]
case class JLongT[F[_]]()   extends JType[F, Long]

case class JFloatT[F[_]]()  extends JType[F, Float]
case class JDoubleT[F[_]]() extends JType[F, Double]

case class JCharT[F[_]]()   extends JType[F, Char]
case class JStrT[F[_]]()    extends JType[F, String]

case class JArrayT[F[_], I](elem: F[I]) extends JType[F, Vector[I]]

object JType {
  type JSchema[I] = HMutu[JType, Schema, I]

  val jNull = prim(HMutu[JType, Schema, Unit](JNullT()))
  val jBool = prim(HMutu[JType, Schema, Boolean](JBoolT()))
  val jInt = prim(HMutu[JType, Schema, Int](JIntT()))
  val jLong = prim(HMutu[JType, Schema, Long](JLongT()))
  val jFloat = prim(HMutu[JType, Schema, Float](JFloatT()))
  val jDouble = prim(HMutu[JType, Schema, Double](JDoubleT()))
  val jChar = prim(HMutu[JType, Schema, Char](JCharT()))
  val jStr = prim(HMutu[JType, Schema, String](JStrT()))

  def jArray[I](elem: Schema[JSchema, I]) = prim(HMutu[JType, Schema, Vector[I]](JArrayT(elem)))

}