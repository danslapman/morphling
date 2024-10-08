# morphling [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
Cats-based Scala library for free applicative schemas. Core module of morphling
initially was a cats-based port of the excellent Kris Nuttycombe's [xenomorph](https://github.com/nuttycom/xenomorph)

# Getting started

All You need is ~~love~~:

```
    libraryDependencies ++= Seq(
      "com.github.danslapman" %% "morphling"               % "4.0.0", //core module
      "com.github.danslapman" %% "morphling-circe"         % "4.0.0",
      "com.github.danslapman" %% "morphling-reactivemongo" % "4.0.0",
      "com.github.danslapman" %% "morphling-typed-schema"  % "4.0.0",
      "com.github.danslapman" %% "morphling-scalacheck"    % "4.0.0",
      "com.github.danslapman" %% "morphling-tapir"         % "4.0.0"
    )
```

# Version compatibility table

| morphling | cats | circe | reactivemongo     | typed-schema | scalacheck | tofu | glass | tapir |
|-----------|------| ----- |-------------------| ------------ | ---------- | ---- | ----- | ----- |
| 4.0 | 2.8 | 0.14.2 | 1.0.3 / 1.1.0-RC6 | 0.14.3 | 1.15.3 | - | 0.3 | 1.0.0 |
| 3.1 | 2.8 | 0.14.2 | 1.0.3 / 1.1.0-RC6 | 0.14.3 | 1.15.3 | - | 0.1 | 1.0.0 |
| 3.0 | 2.8 | 0.14.2 | 1.0.3 / 1.1.0-RC6 | 0.14.3 | 1.15.3 | - | 0.1 | - |
| 2.7-glass | 2.8 | 0.13.0 | 1.0.3             | 0.14.3 | 1.15.3 | - | 0.1 | - |
| 2.7 | 2.4.2 | 0.13.0 | 1.0.3             | 0.14.3 | 1.15.3 | 0.10.0 | - | - |
| 2.6 | 2.4.2 | 0.12.3 | 0.19.3            | 0.12.5.1 | 1.14.3 | 0.7.9 | - | - |
| 2.4 | 2.0.0 | 0.12.3 | 0.19.3            | 0.12.5.1 | 1.14.3 | 0.7.9 | - | - |
| 2.1 | 2.0.0 | 0.12.3 | 0.19.3            | 0.12.4 | 1.14.3 | 0.7.4 | - | - |
| 2.0 | 2.0.0 | 0.12.3 | 0.19.3            | 0.11.1 | 1.14.3 | 0.6.1 | - | - |
| 1.5.1 | 2.0.0 | 0.12.3 | 0.19.3            | 0.11.1 | 1.14.0 | - | - | - |
| 1.5 | 2.0.0 | 0.12.3 | 0.19.0            | 0.11.0 | 1.14.0 | - | - | - |
| 1.1 | 2.0.0 | 0.11.1 | 0.17.0            | 0.11.0-beta6 | 1.14.0 | - | - | - |
| 1.0 | 1.6.1 | 0.11.1 | 0.16.4            | 0.11.0-beta6 | 1.14.0 | - | - | - |

# Setting up protocol
First of all, You need to define a set of "scalar" types You like to support.
They can be `Int`s, `BigInt`s, `Instant`s, any type You mean to treat as scalar, actually.
You can find an example protocol in tests of `core` module:

```scala
import morphling.HMutu
import morphling.Schema._

sealed trait SType[F[_], I]

case class SNullT[F[_]]()   extends SType[F, Unit]
case class SBoolT[F[_]]()   extends SType[F, Boolean]

case class SIntT[F[_]]()    extends SType[F, Int]
case class SLongT[F[_]]()   extends SType[F, Long]

case class SFloatT[F[_]]()  extends SType[F, Float]
case class SDoubleT[F[_]]() extends SType[F, Double]

case class SCharT[F[_]]()   extends SType[F, Char]
case class SStrT[F[_]]()    extends SType[F, String]

case class SArrayT[F[_], I](elem: F[I]) extends SType[F, Vector[I]]
```

Also it will be convenient to define some helper methods (will see their purpose later):
```scala
object SType {
  type SSchema[I] = HMutu[SType, Schema, I]

  val sNull = prim(HMutu[SType, Schema, Unit](SNullT()))
  val sBool = prim(HMutu[SType, Schema, Boolean](SBoolT()))
  val sInt = prim(HMutu[SType, Schema, Int](SIntT()))
  val sLong = prim(HMutu[SType, Schema, Long](SLongT()))
  val sFloat = prim(HMutu[SType, Schema, Float](SFloatT()))
  val sDouble = prim(HMutu[SType, Schema, Double](SDoubleT()))
  val sChar = prim(HMutu[SType, Schema, Char](SCharT()))
  val sStr = prim(HMutu[SType, Schema, String](SStrT()))

  def sArray[I](elem: Schema[SSchema, I]) = prim(HMutu[SType, Schema, Vector[I]](SArrayT(elem)))
}
```

# Creating a Schema

Now we can define a schema for an arbitrary type using our protocol:

```scala
import cats.syntax.apply._
import morphling.Schema
import morphling.Schema._
import glass.macros._
import SType._ //Defined above

case @Optics class Server(host: String, port: Int)
object Server {
  val serverSchema: Schema[SSchema, Server] = rec(
    (
      required("host", sStr, Server.host),
      required("port", sInt, Server.port)
    ).mapN(Server.apply)
  )
}
```

That's it.

# Generating instances

`morphling` provides a set of modules which enables generation of typeclasses
from schema instances. To use them You need to define an "implementation"
of protocol You previously defined. Let's do it for circe Encoding:

```scala
import cats._
import io.circe.{Decoder, Encoder, Json}
import SType.SSchema
import morphling.Schema.Schema

def sTypeEncoder[F[_]: ToJson]: (SType[F, *] ~> Encoder) =
    new (SType[F, *] ~> Encoder) {
      import ToJson._
    
      override def apply[A](st: SType[F, A]): Encoder[A] = st match {
        case SNullT() => Encoder.encodeUnit
        case SBoolT() => Encoder.encodeBoolean
        case SIntT() => Encoder.encodeInt
        case SLongT() => Encoder.encodeLong
        case SFloatT() => Encoder.encodeFloat
        case SDoubleT() => Encoder.encodeDouble
        case SCharT() => Encoder.encodeChar
        case SStrT() => Encoder.encodeString
        case SArrayT(elem) => Encoder.encodeVector(elem.encoder)
      }
    }

implicit val primFromJson: FromJson[SSchema] = new FromJson[SSchema] {
    val decoder = new (SSchema ~> Decoder) {
      def apply[I](s: SSchema[I]): Decoder[I] = sTypeDecoder[SSchema[I]#Inner].apply(s.unmutu)
    }
```

With such a transformation defined we can derive an `Encoder` for `Server`:

```scala
val encoder = Server.schema.encoder
```

[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/com/github/danslapman/morphling_2.13/ "Sonatype Releases"

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.danslapman/morphling_2.13.svg "Sonatype Releases"
