package morphling.samples

import java.time.Instant

val person = Person (
  "Kris Nuttycombe",
  Instant.ofEpochMilli(20147028000L),
  Vector(Administrator("windmill-tilting", 0)),
  42,
  42,
  None
)