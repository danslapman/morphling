package morphling.reactivemongo.annotated

import morphling.reactivemongo.FromBson._
import morphling.reactivemongo.ToBson._
import morphling.reactivemongo.annotated.Implicits._
import morphling.samples.{Person, person}
import morphling.samples.annotated.{AnnPerson, Server}
import morphling.scalacheck.ToGen._
import morphling.scalacheck.annotated.Implicits._
import org.scalacheck.Arbitrary
import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import reactivemongo.api.bson._

import scala.util.Success

class ReactivemongoAnnotatedSpec extends AnyFunSuite with Matchers with TryValues with Checkers {
  private val failure = Symbol("failure")

  test("A value should serialise to BSON") {
    val result = AnnPerson.schema.writer.writeTry(person).success.value
    result shouldBe document(
      "updateCounter" -> 42,
      "roles" -> array(
        document(
          "administrator" -> document(
            "subordinateCount" -> 0,
            "department" -> "windmill-tilting"
          )
        )
      ),
      "birthDate" -> 20147028000L,
      "name" -> "Kris Nuttycombe"
    )
  }

  test("A value should be deserialised from BSON"){
    val result = AnnPerson.schema.writer.writeTry(person).success.value
    AnnPerson.schema.reader.readTry(result) shouldBe Success(person.copy(stamp = 101))
  }

  test("A default value should be applied during deserialization") {
    val result = AnnPerson.schema.writer.writeTry(person).success.value.asInstanceOf[BSONDocument]
    AnnPerson.schema.reader.readTry(result -- "updateCounter") shouldBe Success(person.copy(updateCounter = 0, stamp = 101))
  }

  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(AnnPerson.schema.gen)
    check {
      (p: Person) => AnnPerson.schema.reader.readOpt(AnnPerson.schema.writer.writeTry(p).get) == Some(p)
    }
  }

  test("Deserialization should fail if some value does not fit limitations") {
    val decoder = Server.schema.reader

    decoder.readTry(document("host" -> "peka.com", "port" -> 0)) shouldBe failure
    decoder.readTry(document("host" -> "peka.com", "port" -> 70000)) shouldBe failure
  }
}
