package morphling.scalacheck

import cats._
import morphling.protocol.SType.SSchema
import org.scalacheck.Gen

object Implicits extends GenPack {
  implicit val primToGen: ToGen[SSchema] = new ToGen[SSchema] {
    val toGen = new (SSchema ~> Gen) {
      def apply[I](s: SSchema[I]): Gen[I] = sTypeGen[SSchema[I]#Inner].apply(s.unmutu)
    }
  }
}
