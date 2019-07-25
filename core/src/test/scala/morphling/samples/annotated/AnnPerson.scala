package morphling.samples.annotated

import java.time.Instant

import cats.syntax.apply._
import monocle.Iso
import morphling.annotated.Schema._
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn._
import morphling.samples.Person

object AnnPerson {
  import Restriction.Non

  private val instantIso = Iso[Long, Instant](Instant.ofEpochMilli)(_.toEpochMilli)

  val schema: Schema[ASchema, Person] = rec(
    (
      required("name", sStr(Non), Person.name),
      required(
        "birthDate", sLong(Non).composeIso(instantIso),
        Person.birthDate
      ),
      required("roles", sArray(AnnRole.schema, Non), Person.roles),
      property("updateCounter", sInt(Non), 0, Person.updateCounter),
      constant[ASchema, Restriction]("stamp", 101, Person.stamp),
      absent[ASchema, Restriction]("ignored", Person.ignored)
    ).mapN(Person.apply), Non
  )
}
