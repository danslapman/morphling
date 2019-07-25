package morphling.protocol.annotated

import morphling.HMutu
import morphling.annotated.Schema.{AnnotatedSchema, prim}
import morphling.protocol._

object STypeAnn {
  type Schema[P[_], I] = AnnotatedSchema[P, Restriction, I]

  type ASchema[I] = HMutu[SType, Schema, I]

  def sNull(ann: Restriction): Schema[ASchema, Unit] =
    prim(HMutu[SType, Schema, Unit](SNullT()), ann)
  def sBool(ann: Restriction): Schema[ASchema, Boolean] =
    prim(HMutu[SType, Schema, Boolean](SBoolT()), ann)
  def sInt(ann: Restriction): Schema[ASchema, Int] =
    prim(HMutu[SType, Schema, Int](SIntT()), ann)
  def sLong(ann: Restriction): Schema[ASchema, Long] =
    prim(HMutu[SType, Schema, Long](SLongT()), ann)
  def sFloat(ann: Restriction): Schema[ASchema, Float] =
    prim(HMutu[SType, Schema, Float](SFloatT()), ann)
  def sDouble(ann: Restriction): Schema[ASchema, Double] =
    prim(HMutu[SType, Schema, Double](SDoubleT()), ann)
  def sChar(ann: Restriction): Schema[ASchema, Char] =
    prim(HMutu[SType, Schema, Char](SCharT()), ann)
  def sStr(ann: Restriction): Schema[ASchema, String] =
    prim(HMutu[SType, Schema, String](SStrT()), ann)

  def sArray[I](elem: Schema[ASchema, I], ann: Restriction): Schema[ASchema, Vector[I]] =
    prim(HMutu[SType, Schema, Vector[I]](SArrayT(elem)), ann)
}