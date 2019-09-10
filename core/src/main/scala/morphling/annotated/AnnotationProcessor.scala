package morphling.annotated

import cats.~>

trait AnnotationProcessor[Ann, Target[_]] {
  def process: Ann => Target ~> Target
}

object AnnotationProcessor {
  def apply[Ann, Target[_]](implicit ap: AnnotationProcessor[Ann, Target]): AnnotationProcessor[Ann, Target] = ap
}
