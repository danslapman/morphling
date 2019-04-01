package morphling.tschema

import cats.Eq
import cats.instances.function._
import io.circe.literal._
import io.circe.syntax._
import morphling.samples.Person
import morphling.tschema.Implicits._
import morphling.tschema.ToTypeable._
import org.scalactic.Equality
import org.scalatest.{FunSuite, Matchers}

import scala.reflect.ClassTag

class TSchemaSpec extends FunSuite with Matchers {
  implicit def eqEquality[T: Eq : ClassTag]: Equality[T] =
    (a: T, b: Any) => b match {
      case bt: T => Eq.eqv(a, bt)
      case _ => false
    }

  test("Typeable should be generated") {
    val personTypeableJson = Person.schema.toTypeable.typ.asJson.dropNulls.run

    personTypeableJson shouldEqual
      json"""{
               "type" : "object",
               "required" : [
                 "name",
                 "birthDate",
                 "roles"
               ],
               "properties" : {
                 "roles" : {
                   "type" : "array",
                   "items" : {
                     "type" : "object",
                     "oneOf" : [
                       {
                         "type" : "object",
                         "required" : [
                           "user"
                         ],
                         "properties" : {
                           "user" : {
                             "type" : "object",
                             "required" : [
                             ],
                             "properties" : {

                             }
                           }
                         }
                       },
                       {
                         "type" : "object",
                         "required" : [
                           "administrator"
                         ],
                         "properties" : {
                           "administrator" : {
                             "type" : "object",
                             "required" : [
                               "department",
                               "subordinateCount"
                             ],
                             "properties" : {
                               "department" : {
                                 "type" : "string"
                               },
                               "subordinateCount" : {
                                 "format" : "int32",
                                 "type" : "integer"
                               }
                             }
                           }
                         }
                       }
                     ]
                   }
                 },
                 "name" : {
                   "type" : "string"
                 },
                 "birthDate" : {
                   "format" : "int64",
                   "type" : "integer"
                 }
               }
             }
        """
  }

  test("Flat typeable should be generated") {
    val personTypeableJson = Person.flatSchema.toTypeable.typ.asJson.dropNulls.run

    personTypeableJson shouldEqual
      json"""{
                 "type" : "object",
                 "required" : [
                   "name",
                   "birthDate",
                   "roles"
                 ],
                 "properties" : {
                   "roles" : {
                     "type" : "array",
                     "items" : {
                       "type" : "object",
                       "oneOf" : [
                         {
                           "type" : "object",
                           "required" : [
                             "type"
                           ],
                           "properties" : {
                             "type" : {
                               "type" : "string"
                             }
                           }
                         },
                         {
                           "type" : "object",
                           "required" : [
                             "department",
                             "subordinateCount",
                             "type"
                           ],
                           "properties" : {
                             "department" : {
                               "type" : "string"
                             },
                             "subordinateCount" : {
                               "format" : "int32",
                               "type" : "integer"
                             },
                             "type" : {
                               "type" : "string"
                             }
                           }
                         }
                       ],
                       "discriminator" : {
                         "propertyName" : "type",
                         "mapping" : {

                         }
                       }
                     }
                   },
                   "name" : {
                     "type" : "string"
                   },
                   "birthDate" : {
                     "format" : "int64",
                     "type" : "integer"
                   }
                 }
               }
        """
  }
}
