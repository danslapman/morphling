package morphling.samples

import cats.syntax.apply._
import monocle.macros.{GenLens, GenPrism}
import morphling.Schema
import morphling.Schema._
import morphling.protocol.SType._
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
  val prism = GenPrism[Role, User.type]
}

case class Administrator(department: String, subordinateCount: Int) extends Role
object Administrator {
  val department = GenLens[Administrator](_.department)
  val subordinateCount = GenLens[Administrator](_.subordinateCount)
  val prism = GenPrism[Role, Administrator]
}
