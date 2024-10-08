package morphling.samples

import cats.data.NonEmptyList
import cats.syntax.apply.*
import glass.macros.*
import morphling.Schema
import morphling.Schema.*
import morphling.protocol.SType.*

sealed trait Role

object Role {
  val schema: Schema[SSchema, Role] = Schema.unsafeOneOf(
    NonEmptyList.of(
      alt[SSchema, Role, User.type](
        "user",
        Schema.const(User),
        User.prism
      ),
      alt[SSchema, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr, Administrator.department),
            required("subordinateCount", sInt, Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      )
    )
  )

  val flatSchema: Schema[SSchema, Role] = Schema.unsafeOneOfDiscr("type")(
    NonEmptyList.of(
      alt[SSchema, Role, User.type](
        "user",
        Schema.const(User),
        User.prism
      ),
      alt[SSchema, Role, Administrator](
        "administrator",
        rec(
          (
            required("department", sStr, Administrator.department),
            required("subordinateCount", sInt, Administrator.subordinateCount)
          ).mapN(Administrator.apply)
        ),
        Administrator.prism
      )
    )
  )
}

case object User extends Role {
  val prism = GenSubset[Role, User.type]
}

case class Administrator(department: String, subordinateCount: Int) extends Role
object Administrator extends DeriveContains {
  val prism = GenSubset[Role, Administrator]
}
