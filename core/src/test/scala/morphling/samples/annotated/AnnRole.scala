package morphling.samples.annotated

import cats.syntax.apply._
import morphling.annotated.Schema
import morphling.annotated.Schema._
import morphling.protocol.annotated.{NoRestr, Restriction}
import morphling.protocol.annotated.STypeAnn._
import morphling.samples.{Administrator, Role, User}
import shapeless.HNil

object AnnRole {
  import Restriction.Non

  val schema: Schema[ASchema, Role] = Schema.oneOf(NoRestr: Restriction)(
    alt[ASchema, Restriction, Role, User.type](
      "user",
      Schema.const(User, Non),
      User.prism
    ) ::
      alt[ASchema, Restriction, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr(Non), Administrator.department),
            required("subordinateCount", sInt(Non), Administrator.subordinateCount)
          ).mapN(Administrator.apply), Non
        ),
        Administrator.prism
      ) :: HNil
  )
}
