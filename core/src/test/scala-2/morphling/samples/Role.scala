package morphling.samples

import cats.syntax.apply.*
import glass.macros.*
import morphling.Schema
import morphling.Schema.*
import morphling.protocol.SType.*
import shapeless.HNil

sealed trait Role

object Role {
  val schema: Schema[SSchema, Role] = Schema.oneOf(
    alt[SSchema, Role, User.type](
      "user",
      Schema.const(User),
      User.prism
    ) ::
      alt[SSchema, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr, Administrator.department),
            required("subordinateCount", sInt, Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      ) :: HNil
  )

  val flatSchema: Schema[SSchema, Role] = Schema.oneOfDiscr("type")(
    alt[SSchema, Role, User.type](
      "user",
      Schema.const(User),
      User.prism
    ) ::
      alt[SSchema, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr, Administrator.department),
            required("subordinateCount", sInt, Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      ) :: HNil
  )
}

case object User extends Role {
  val prism = GenSubset[Role, User.type]
}

@Optics
case class Administrator(department: String, subordinateCount: Int) extends Role
object Administrator {
  val prism = GenSubset[Role, Administrator]
}
