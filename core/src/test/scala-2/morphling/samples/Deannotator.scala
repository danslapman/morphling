package morphling.samples

import cats.*
import morphling.HFix
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import morphling.protocol.SType.SSchema
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.ASchema

object Deannotator extends (AnnotatedSchema[ASchema, Restriction, *] ~> Schema[SSchema, *]) {
  override def apply[T](as: AnnotatedSchema[ASchema, Restriction, T]): Schema[SSchema, T] = {
    HFix.hfix(as.unfix.value.fa.pmap[SSchema](λ[ASchema ~> SSchema](_.transformInner[Schema](Deannotator))).hfmap(Deannotator))
  }
}