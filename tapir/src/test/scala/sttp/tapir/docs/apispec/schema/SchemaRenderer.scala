package sttp.tapir.docs.apispec.schema

import sttp.apispec.{ReferenceOr, Schema as ASchema}
import sttp.tapir.Schema

object SchemaRenderer {
  private val nsr  = new NameToSchemaReference(Map.empty)
  private val sToS = new TSchemaToASchema(nsr, true)

  def convert[T](schema: Schema[T]): ReferenceOr[ASchema] = sToS(schema)
}
