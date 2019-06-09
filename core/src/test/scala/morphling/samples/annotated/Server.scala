package morphling.samples.annotated

import cats.syntax.apply._
import monocle.macros.GenLens
import morphling.{HFix, HFunctor, SchemaF}
import morphling.HEnvT._
import morphling.HFix._
import morphling.annotated.Schema._
import morphling.Schema.{Schema => PlainSchema}
import morphling.protocol.SType
import morphling.protocol.SType.SSchema
import morphling.protocol.annotated.SType._

case class Server(host: String, port: Int)
object Server {
  val host = GenLens[Server](_.host)
  val port = GenLens[Server](_.port)

  val schema: Schema[ASchema[Restriction, ?], Restriction, Server] = rec(
    (
      required("host", sStr(NoRestr: Restriction), host.asGetter),
      required("port", sInt(Range(1, 65535): Restriction), port.asGetter)
    ).mapN(Server.apply),
    NoRestr: Restriction
  )

  val unSchema = //: HFix[SchemaF[PSSchema, ?[_], ?], Server] = //: PlainSchema[PSSchema, Server] =
    HFix.forget[SchemaF[ASchema[Restriction, ?], ?[_], ?], Restriction]
      .apply(schema)

}
