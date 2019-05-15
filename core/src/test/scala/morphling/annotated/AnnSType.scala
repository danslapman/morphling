package morphling.annotated

import morphling.AnnSchema._
import morphling.HMutu
import morphling.protocol._

object AnnSType {
  type AnnSSchema[I] = HMutu[SType, AnnSchema[?[_], Restriction, ?], I]

  def sNull(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Unit](SNullT()), ann)
  def sBool(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Boolean](SBoolT()), ann)
  def sInt(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Int](SIntT()), ann)
  def sLong(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Long](SLongT()), ann)
  def sFloat(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Float](SFloatT()), ann)
  def sDouble(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Double](SDoubleT()), ann)
  def sChar(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Char](SCharT()), ann)
  def sStr(ann: Restriction = NoRestr) = prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], String](SStrT()), ann)

  def sArray[I](elem: AnnSchema[AnnSSchema, Restriction, I]): AnnSchema[AnnSSchema, Restriction, Vector[I]] =
    prim(HMutu[SType, AnnSchema[?[_], Restriction, ?], Vector[I]](SArrayT(elem)), NoRestr)
}
