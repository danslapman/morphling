package morphling.samples.annotated

import java.time.Instant

import cats.syntax.apply.*
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Restriction
import morphling.protocol.annotated.STypeAnn.*
import morphling.samples.Person
import glass.Equivalent

object AnnPerson {
  private val instantIso = Equivalent[Long](Instant.ofEpochMilli _)(_.toEpochMilli)

  val schema: Schema[ASchema, Person] = rec(
    (
      required("name", sStr(), Person.name),
      required(
        "birthDate", sLong().composeIso(instantIso, _.asInstanceOf[Restriction[Instant]]),
        Person.birthDate
      ),
      required("roles", sArray(AnnRole.schema), Person.roles),
      property("updateCounter", sInt(), 0, Person.updateCounter),
      constant[ASchema, Restriction]("stamp", 101, Person.stamp),
      absent[ASchema, Restriction]("ignored", Person.ignored)
    ).mapN(Person.apply)
  )
}
