package morphling.samples.annotated

import cats.syntax.apply.*
import glass.macros.*
import morphling.annotated.Schema.*
import morphling.protocol.annotated.Range
import morphling.protocol.annotated.STypeAnn.*

case class Server(host: String, port: Int)
object Server extends DeriveContains {
  val schema: Schema[ASchema, Server] = rec(
    (
      required("host", sStr(), this.host),
      required("port", sInt(Range(1, 65535)), this.port)
    ).mapN(Server.apply)
  )
}
