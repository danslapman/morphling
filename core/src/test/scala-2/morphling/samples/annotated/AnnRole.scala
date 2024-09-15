package morphling.samples.annotated

import cats.syntax.apply.*
import morphling.annotated.Schema
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.*
import morphling.samples.{Administrator, Role, User}
import shapeless.HNil

object AnnRole {
  val schema: Schema[ASchema, Role] = Schema.oneOf(
    alt[ASchema, Restriction, Role, User.type](
      "user",
      Schema.const(User),
      User.prism
    ) ::
      alt[ASchema, Restriction, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr(), Administrator.department),
            required("subordinateCount", sInt(), Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      ) :: HNil
  )
}
