package morphling.reactivemongo

case class MultipleKeysFound(message: String) extends Exception
case class TypeDoesNotMatch(message: String) extends Exception
case class DocumentKeyNotFound(message: String) extends Exception