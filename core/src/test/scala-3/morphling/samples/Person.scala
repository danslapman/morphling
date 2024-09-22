package morphling.samples

import cats.syntax.apply.*
import glass.*
import glass.macros.*
import morphling.Schema.*
import morphling.protocol.SType.*
import morphling.samples.annotated.AnnPerson

import java.time.Instant

case class Person(
    name: String,
    birthDate: Instant,
    roles: Vector[Role],
    updateCounter: Int,
    stamp: Int,
    ignored: Option[Any]
)

object Person extends DeriveContains {
  private val instantIso = Equivalent[Long](Instant.ofEpochMilli(_))(_.toEpochMilli)

  val schema: Schema[SSchema, Person] = rec(
    (
      required("name", sStr, Person.name),
      required(
        "birthDate",
        sLong.composeIso(instantIso),
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
        "birthDate",
        sLong.composeIso(instantIso),
        Person.birthDate
      ),
      required("roles", sArray(Role.flatSchema), Person.roles),
      property("updateCounter", sInt, 0, Person.updateCounter),
      constant[SSchema]("stamp", 101, Person.stamp),
      absent[SSchema]("ignored", Person.ignored)
    ).mapN(Person.apply)
  )

  lazy val deannotatedSchema: Schema[SSchema, Person] =
    Deannotator(AnnPerson.schema)
}
