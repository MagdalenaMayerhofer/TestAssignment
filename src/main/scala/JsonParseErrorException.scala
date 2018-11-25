final case class JsonParseErrorException(private val message: String = "") extends Exception(message)
