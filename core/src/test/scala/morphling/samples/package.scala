package morphling

import java.time.Instant

package object samples {
  val person = Person(
    "Kris Nuttycombe",
    Instant.ofEpochMilli(20147028000l),
    Vector(Administrator("windmill-tilting", 0)),
    42,
    42,
    None
  )
}
