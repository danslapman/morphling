package morphling.protocol.annotated

import alleycats.EmptyK
import morphling.HMutu
import morphling.annotated.Schema.{AnnotatedSchema, prim}
import morphling.protocol._

object STypeAnn {
  type Schema[P[_], I] = AnnotatedSchema[P, Restriction, I]

  type ASchema[I] = HMutu[SType, Schema, I]

  private def non[T]: Restriction[T] = EmptyK[Restriction].empty[T]

  def sNull(ann: Restriction[Unit] = non): Schema[ASchema, Unit] =
    prim(HMutu[SType, Schema, Unit](SNullT()), ann)
  def sBool(ann: Restriction[Boolean] = non): Schema[ASchema, Boolean] =
    prim(HMutu[SType, Schema, Boolean](SBoolT()), ann)
  def sInt(ann: Restriction[Int] = non): Schema[ASchema, Int] =
    prim(HMutu[SType, Schema, Int](SIntT()), ann)
  def sLong(ann: Restriction[Long] = non): Schema[ASchema, Long] =
    prim(HMutu[SType, Schema, Long](SLongT()), ann)
  def sFloat(ann: Restriction[Float] = non): Schema[ASchema, Float] =
    prim(HMutu[SType, Schema, Float](SFloatT()), ann)
  def sDouble(ann: Restriction[Double] = non): Schema[ASchema, Double] =
    prim(HMutu[SType, Schema, Double](SDoubleT()), ann)
  def sChar(ann: Restriction[Char] = non): Schema[ASchema, Char] =
    prim(HMutu[SType, Schema, Char](SCharT()), ann)
  def sStr(ann: Restriction[String] = non): Schema[ASchema, String] =
    prim(HMutu[SType, Schema, String](SStrT()), ann)

  def sArray[I](elem: Schema[ASchema, I], ann: Restriction[Vector[I]] = non[Vector[I]]): Schema[ASchema, Vector[I]] =
    prim(HMutu[SType, Schema, Vector[I]](SArrayT(elem)), ann)
}