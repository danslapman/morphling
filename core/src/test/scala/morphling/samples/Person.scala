package morphling.samples

import cats.syntax.apply._
import monocle.macros.GenLens
import morphling.Schema
import morphling.Schema._
import morphling.protocol.SType._
import java.time.Instant

import monocle.Iso

case class Person(
  name: String,
  birthDate: Instant,
  roles: Vector[Role],
  updateCounter: Int
)

object Person {
  val name = GenLens[Person](_.name)
  val birthDate = GenLens[Person](_.birthDate)
  val roles = GenLens[Person](_.roles)
  val updateCounter = GenLens[Person](_.updateCounter)

  val schema: Schema[SSchema, Person] = rec(
    (
      required("name", sStr, Person.name.asGetter),
      required(
        "birthDate", sLong.composeIso(Iso[Long, Instant](Instant.ofEpochMilli)(_.toEpochMilli)),
        Person.birthDate.asGetter
      ),
      required("roles", sArray(Role.schema), Person.roles.asGetter),
      property("updateCounter", sInt, 0, Person.updateCounter.asGetter)
    ).mapN(Person.apply)
  )

  val flatSchema: Schema[SSchema, Person] = rec(
    (
      required("name", sStr, Person.name.asGetter),
      required(
        "birthDate", sLong.composeIso(Iso[Long, Instant](Instant.ofEpochMilli)(_.toEpochMilli)),
        Person.birthDate.asGetter
      ),
      required("roles", sArray(Role.flatSchema), Person.roles.asGetter),
      property("updateCounter", sInt, 0, Person.updateCounter.asGetter)
    ).mapN(Person.apply)
  )
}
