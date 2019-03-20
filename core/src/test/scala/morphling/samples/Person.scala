package morphling.samples

import cats.syntax.apply._
import monocle.macros.{GenLens, GenPrism}
import morphling.Schema
import morphling.Schema._
import morphling.json.JType._
import shapeless.HNil
import java.time.Instant

import monocle.Iso

case class Person(
  name: String,
  birthDate: Instant,
  roles: Vector[Role]
)

object Person {
  val name = GenLens[Person](_.name)
  val birthDate = GenLens[Person](_.birthDate)
  val roles = GenLens[Person](_.roles)

  val schema: Schema[JSchema, Person] = rec(
    (
      required("name", jStr, Person.name.asGetter),
      required(
        "birthDate", jLong.composeIso(Iso(Instant.ofEpochMilli(_:Long))((_ : Instant).toEpochMilli)),
        Person.birthDate.asGetter
      ),
      required("roles", jArray(Role.schema), Person.roles.asGetter)
    ).mapN(Person.apply)
  )
}
