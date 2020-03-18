package morphling

import java.time.Instant

package object samples {
  val person = Person(
    "Kris Nuttycombe",
    Instant.ofEpochMilli(20147028000L),
    Vector(Administrator("windmill-tilting", 0)),
    42,
    42,
    None
  )
}
