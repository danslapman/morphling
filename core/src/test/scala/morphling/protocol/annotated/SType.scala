package morphling.protocol.annotated

import morphling.HMutu
import morphling.annotated.Schema.{Schema, prim}
import morphling.protocol._

object SType {
  type SSchema[A, I] = HMutu[SType, Schema[?[_], A, ?], I]

  def sNull[A](ann: A): Schema[SSchema[A, ?], A, Unit] =
    prim(HMutu[SType, Schema[?[_], A, ?], Unit](SNullT()), ann)
  def sBool[A](ann: A): Schema[SSchema[A, ?], A, Boolean] =
    prim(HMutu[SType, Schema[?[_], A, ?], Boolean](SBoolT()), ann)
  def sInt[A](ann: A): Schema[SSchema[A, ?], A, Int] =
    prim(HMutu[SType, Schema[?[_], A, ?], Int](SIntT()), ann)
  def sLong[A](ann: A): Schema[SSchema[A, ?], A, Long] =
    prim(HMutu[SType, Schema[?[_], A, ?], Long](SLongT()), ann)
  def sFloat[A](ann: A): Schema[SSchema[A, ?], A, Float] =
    prim(HMutu[SType, Schema[?[_], A, ?], Float](SFloatT()), ann)
  def sDouble[A](ann: A): Schema[SSchema[A, ?], A, Double] =
    prim(HMutu[SType, Schema[?[_], A, ?], Double](SDoubleT()), ann)
  def sChar[A](ann: A): Schema[SSchema[A, ?], A, Char] =
    prim(HMutu[SType, Schema[?[_], A, ?], Char](SCharT()), ann)
  def sStr[A](ann: A): Schema[SSchema[A, ?], A, String] =
    prim(HMutu[SType, Schema[?[_], A, ?], String](SStrT()), ann)

  def sArray[I, A](elem: Schema[SSchema[A, ?], A, I], ann: A): Schema[SSchema[A, ?], A, Vector[I]] =
    prim(HMutu[SType, Schema[?[_], A, ?], Vector[I]](SArrayT(elem)), ann)
}
