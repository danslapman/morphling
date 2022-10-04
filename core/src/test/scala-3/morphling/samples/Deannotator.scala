package morphling.samples

import cats.*
import cats.arrow.*
import morphling.HFix
import morphling.Schema.Schema
import morphling.annotated.Schema.AnnotatedSchema
import morphling.protocol.SType.SSchema
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.ASchema

object Deannotator extends (AnnotatedSchema[ASchema, Restriction, _] ~> Schema[SSchema, _]) {
  override def apply[T](as: AnnotatedSchema[ASchema, Restriction, T]): Schema[SSchema, T] =
    HFix.hfix(
      as.unfix.value.fa
        .pmap[SSchema](
          FunctionK.lift[ASchema, SSchema]([T] => (_: ASchema[T]).transformInner[Schema](Deannotator))
        )
        .hfmap(Deannotator)
    )
}
