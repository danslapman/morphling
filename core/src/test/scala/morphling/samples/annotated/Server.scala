package morphling.samples.annotated

import cats.syntax.apply._
import monocle.macros.GenLens
import Schema._
import morphling.protocol.annotated.SType._

case class Server(host: String, port: Int)
object Server {
  val host = GenLens[Server](_.host)
  val port = GenLens[Server](_.port)

  val schema: Schema[SSchema, Restriction, Server] = rec(
    (
      required("host", sStr(), host.asGetter),
      required("port", sInt(Range(1, 65535)), port.asGetter)
    ).mapN(Server.apply),
    NoRestr
  )
}
