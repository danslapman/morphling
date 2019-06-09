package morphling.protocol.annotated

import morphling.HMutu
import morphling.annotated.Schema.{Schema, prim}
import morphling.samples.annotated.{NoRestr, Restriction}
import morphling.protocol._

object SType {
  type SSchema[I] = HMutu[SType, Schema[?[_], Restriction, ?], I]

  def sNull(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Unit] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Unit](SNullT()), ann)
  def sBool(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Boolean] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Boolean](SBoolT()), ann)
  def sInt(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Int] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Int](SIntT()), ann)
  def sLong(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Long] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Long](SLongT()), ann)
  def sFloat(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Float] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Float](SFloatT()), ann)
  def sDouble(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Double] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Double](SDoubleT()), ann)
  def sChar(ann: Restriction = NoRestr): Schema[SSchema, Restriction, Char] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Char](SCharT()), ann)
  def sStr(ann: Restriction = NoRestr): Schema[SSchema, Restriction, String] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], String](SStrT()), ann)

  def sArray[I](elem: Schema[SSchema, Restriction, I]): Schema[SSchema, Restriction, Vector[I]] =
    prim(HMutu[SType, Schema[?[_], Restriction, ?], Vector[I]](SArrayT(elem)), NoRestr)
}
