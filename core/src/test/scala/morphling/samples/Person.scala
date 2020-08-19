package morphling.samples

import java.time.Instant

import cats.syntax.apply._
import morphling.Schema._
import morphling.protocol.SType._
import tofu.optics._
import tofu.optics.macros._

case class Person(
  name: String,
  birthDate: Instant,
  roles: Vector[Role],
  updateCounter: Int,
  stamp: Int,
  ignored: Option[Any]
)

object Person {
  val name = GenContains[Person](_.name)
  val birthDate = GenContains[Person](_.birthDate)
  val roles = GenContains[Person](_.roles)
  val updateCounter = GenContains[Person](_.updateCounter)
  val stamp = GenContains[Person](_.stamp)
  val ignored: Contains[Person, Option[Any]] = GenContains[Person](_.ignored)

  private val instantIso = Equivalent[Long](Instant.ofEpochMilli)(_.toEpochMilli)

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
