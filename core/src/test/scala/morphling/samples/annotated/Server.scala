package morphling.samples.annotated

import cats.syntax.apply._
import monocle.macros.GenLens
import morphling.annotated.Schema._
import morphling.protocol.annotated.Range
import morphling.protocol.annotated.STypeAnn._

case class Server(host: String, port: Int)
object Server {
  val host = GenLens[Server](_.host)
  val port = GenLens[Server](_.port)

  val schema: Schema[ASchema, Server] = rec(
    (
      required("host", sStr(), host),
      required("port", sInt(Range(1, 65535)), port)
    ).mapN(Server.apply)
  )
}
