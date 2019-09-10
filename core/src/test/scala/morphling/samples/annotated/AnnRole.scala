package morphling.samples.annotated

import cats.syntax.apply._
import morphling.annotated.Schema
import morphling.annotated.Schema._
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn._
import morphling.samples.{Administrator, Role, User}
import shapeless.HNil

object AnnRole {
  import Restriction.non

  val schema: Schema[ASchema, Role] = Schema.oneOf(non[Role])(
    alt[ASchema, Restriction, Role, User.type](
      "user",
      Schema.const(User, non),
      User.prism
    ) ::
      alt[ASchema, Restriction, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr(non), Administrator.department),
            required("subordinateCount", sInt(non), Administrator.subordinateCount)
          ).mapN(Administrator.apply), non
        ),
        Administrator.prism
      ) :: HNil
  )
}
