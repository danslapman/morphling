package morphling.samples.annotated

import java.time.Instant

import cats.syntax.apply._
import monocle.Iso
import morphling.annotated.Schema._
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn._
import morphling.samples.{Person, Role}

object AnnPerson {
  import Restriction.non

  private val instantIso = Iso[Long, Instant](Instant.ofEpochMilli)(_.toEpochMilli)

  val schema: Schema[ASchema, Person] = rec(
    (
      required("name", sStr(non), Person.name),
      required(
        "birthDate", sLong(non).composeIso(instantIso, _.asInstanceOf[Restriction[Instant]]),
        Person.birthDate
      ),
      required("roles", sArray(AnnRole.schema, non[Vector[Role]]), Person.roles),
      property("updateCounter", sInt(non), 0, Person.updateCounter),
      constant[ASchema, Restriction]("stamp", 101, Person.stamp),
      absent[ASchema, Restriction]("ignored", Person.ignored)
    ).mapN(Person.apply), non
  )
}
