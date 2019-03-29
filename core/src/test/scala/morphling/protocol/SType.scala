package morphling.protocol

import morphling.HMutu
import morphling.Schema._

sealed trait SType[F[_], I]

case class SNullT[F[_]]()   extends SType[F, Unit]
case class SBoolT[F[_]]()   extends SType[F, Boolean]

case class SIntT[F[_]]()    extends SType[F, Int]
case class SLongT[F[_]]()   extends SType[F, Long]

case class SFloatT[F[_]]()  extends SType[F, Float]
case class SDoubleT[F[_]]() extends SType[F, Double]

case class SCharT[F[_]]()   extends SType[F, Char]
case class SStrT[F[_]]()    extends SType[F, String]

case class SArrayT[F[_], I](elem: F[I]) extends SType[F, Vector[I]]

object SType {
  type SSchema[I] = HMutu[SType, Schema, I]

  val jNull = prim(HMutu[SType, Schema, Unit](SNullT()))
  val jBool = prim(HMutu[SType, Schema, Boolean](SBoolT()))
  val jInt = prim(HMutu[SType, Schema, Int](SIntT()))
  val jLong = prim(HMutu[SType, Schema, Long](SLongT()))
  val jFloat = prim(HMutu[SType, Schema, Float](SFloatT()))
  val jDouble = prim(HMutu[SType, Schema, Double](SDoubleT()))
  val jChar = prim(HMutu[SType, Schema, Char](SCharT()))
  val jStr = prim(HMutu[SType, Schema, String](SStrT()))

  def jArray[I](elem: Schema[SSchema, I]) = prim(HMutu[SType, Schema, Vector[I]](SArrayT(elem)))

}