package morphling.samples.annotated

import cats.syntax.apply.*
import glass.macros.*
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Range
import morphling.protocol.annotated.STypeAnn.*

case class Server(host: String, port: Int)
object Server {
  val host = GenContains[Server](_.host)
  val port = GenContains[Server](_.port)

  val schema: Schema[ASchema, Server] = rec(
    (
      required("host", sStr(), host),
      required("port", sInt(Range(1, 65535)), port)
    ).mapN(Server.apply)
  )
}
