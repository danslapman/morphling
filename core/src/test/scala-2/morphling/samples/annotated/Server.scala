package morphling.samples.annotated

import cats.syntax.apply.*
import glass.macros.*
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Range
import morphling.protocol.annotated.STypeAnn.*

@Optics
case class Server(host: String, port: Int)
object Server {
  val schema: Schema[ASchema, Server] = rec(
    (
      required("host", sStr(), host),
      required("port", sInt(Range(1, 65535)), port)
    ).mapN(Server.apply)
  )
}
