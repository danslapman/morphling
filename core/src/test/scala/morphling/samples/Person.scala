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
  updateCounter: Int,
  stamp: Int,
  ignored: Option[Any]
)

object Person {
  val name = GenLens[Person](_.name)
  val birthDate = GenLens[Person](_.birthDate)
  val roles = GenLens[Person](_.roles)
  val updateCounter = GenLens[Person](_.updateCounter)
  val stamp = GenLens[Person](_.stamp)
  val ignored = GenLens[Person](_.ignored)

  private val instantIso = Iso[Long, Instant](Instant.ofEpochMilli)(_.toEpochMilli)

  val schema: Schema[SSchema, Person] = rec(
    (
      required("name", sStr, Person.name),
      required(
        "birthDate", sLong.composeIso(instantIso),
        Person.birthDate
      ),
      required("roles", sArray(Role.schema), Person.roles),
      property("updateCounter", sInt, 0, Person.updateCounter),
      constant[SSchema]("stamp", 101, Person.stamp),
      absent[SSchema]("ignored", Person.ignored)
    ).mapN(Person.apply)
  )

  val flatSchema: Schema[SSchema, Person] = rec(
    (
      required("name", sStr, Person.name),
      required(
        "birthDate", sLong.composeIso(instantIso),
        Person.birthDate
      ),
      required("roles", sArray(Role.flatSchema), Person.roles),
      property("updateCounter", sInt, 0, Person.updateCounter),
      constant[SSchema]("stamp", 101, Person.stamp),
      absent[SSchema]("ignored", Person.ignored)
    ).mapN(Person.apply)
  )
}
