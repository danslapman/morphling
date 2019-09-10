package morphling.protocol.annotated

import morphling.HMutu
import morphling.annotated.Schema.{AnnotatedSchema, prim}
import morphling.protocol._

object STypeAnn {
  type Schema[P[_], I] = AnnotatedSchema[P, Restriction, I]

  type ASchema[I] = HMutu[SType, Schema, I]

  def sNull(ann: Restriction[Unit]): Schema[ASchema, Unit] =
    prim(HMutu[SType, Schema, Unit](SNullT()), ann)
  def sBool(ann: Restriction[Boolean]): Schema[ASchema, Boolean] =
    prim(HMutu[SType, Schema, Boolean](SBoolT()), ann)
  def sInt(ann: Restriction[Int]): Schema[ASchema, Int] =
    prim(HMutu[SType, Schema, Int](SIntT()), ann)
  def sLong(ann: Restriction[Long]): Schema[ASchema, Long] =
    prim(HMutu[SType, Schema, Long](SLongT()), ann)
  def sFloat(ann: Restriction[Float]): Schema[ASchema, Float] =
    prim(HMutu[SType, Schema, Float](SFloatT()), ann)
  def sDouble(ann: Restriction[Double]): Schema[ASchema, Double] =
    prim(HMutu[SType, Schema, Double](SDoubleT()), ann)
  def sChar(ann: Restriction[Char]): Schema[ASchema, Char] =
    prim(HMutu[SType, Schema, Char](SCharT()), ann)
  def sStr(ann: Restriction[String]): Schema[ASchema, String] =
    prim(HMutu[SType, Schema, String](SStrT()), ann)

  def sArray[I](elem: Schema[ASchema, I], ann: Restriction[Vector[I]]): Schema[ASchema, Vector[I]] =
    prim(HMutu[SType, Schema, Vector[I]](SArrayT(elem)), ann)
}