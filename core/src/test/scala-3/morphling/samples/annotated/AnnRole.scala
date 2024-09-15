package morphling.samples.annotated

import cats.data.NonEmptyList
import cats.syntax.apply.*
import morphling.annotated.Schema
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.*
import morphling.samples.{Administrator, Role, User}

object AnnRole {
  val schema: Schema[ASchema, Role] = Schema.unsafeOneOf(
    NonEmptyList.of(
      alt[ASchema, Restriction, Role, User.type](
        "user",
        Schema.const(User),
        User.prism
      ),
      alt[ASchema, Restriction, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr(), Administrator.department),
            required("subordinateCount", sInt(), Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      )
    )
  )
}
