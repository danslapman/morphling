package morphling.samples

import cats.syntax.apply._
import monocle.macros.{GenLens, GenPrism}
import morphling.Schema
import morphling.Schema._
import morphling.protocol.JType._
import shapeless.HNil

sealed trait Role

object Role {
  val schema: Schema[JSchema, Role] = Schema.oneOf(
    alt[JSchema, Role, User.type](
      "user",
      Schema.const(User),
      User.prism
    ) ::
    alt[JSchema, Role, Administrator](
      "administrator",
      rec(
        (
          required("department", jStr, Administrator.department.asGetter),
          required("subordinateCount", jInt, Administrator.subordinateCount.asGetter)
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
