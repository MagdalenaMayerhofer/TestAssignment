import com.fasterxml.jackson.core.JsonParseException
import play.api.libs.json._

object Parser {

  /***
    * This function returns the BooleanExpression if it got an valid input string,
    * otherwise it throws an exception.
    * @param json: the input string you want to convert into an BooleanExpression
    * @return if there are no errors it returns the BooleanExpression
    */
  def parseJsonToBooleanExpression(json: String): BooleanExpression = {
    try {
      val jsValue = Json.parse(json)
      buildBooleanExpression(jsValue)
    }
    catch {
      case _: JsonParseException => throw JsonParseErrorException("Not a valid JSON format!")
    }
  }

  def buildBooleanExpression(json: JsValue): BooleanExpression = try {
    (json \ "type").get match {
      case JsString("true") => True
      case JsString("false") => False
      case JsString("variable") =>
        try {
          Variable((json \ "symbol").get.as[String])
        }
        catch {
          case _: NoSuchElementException =>
            throw JsonParseErrorException("A variable needs a symbol!")
        }
      case JsString("not") =>
        try {
          Not(buildBooleanExpression((json \ "e").get))
        }
        catch {
          case ex: NoSuchElementException =>
            throw JsonParseErrorException("A 'not' needs an expression e!")
        }
      case JsString("or") =>
        try {
          Or(buildBooleanExpression((json \ "e1").get), buildBooleanExpression((json \ "e2").get))
        }
        catch {
          case ex: NoSuchElementException =>
            throw JsonParseErrorException("A 'or' needs two expressions: e1 and e2!")
        }
      case JsString("and") =>
        try {
          And(buildBooleanExpression((json \ "e1").get), buildBooleanExpression((json \ "e2").get))
        }
        catch {
          case _: NoSuchElementException => throw JsonParseErrorException("A 'and' needs two expressions: e1 and e2!")
        }
      case _ => throw JsonParseErrorException("Not a valid boolean expression type!")
    }
  }
  catch {
    case _: NoSuchElementException => throw JsonParseErrorException("The JSON needs a boolean expression type!")
  }

  /***
    * This function returns the JSON sting for the given BooleanExpression.
    * @param booleanExpression: the BooleanExpression you want to convert to a JSON
    * @return the JSON string
    */
  def parseBooleanExpressionToJson(booleanExpression: BooleanExpression): String = {
    buildJson(booleanExpression).toString()
  }

  def buildJson(booleanExpression: BooleanExpression): JsObject = booleanExpression match {
    case True => JsObject(Seq(
      "type" -> JsString("true")
    ))
    case False => JsObject(Seq(
      "type" -> JsString("false")
    ))
    case Variable(symbol) => JsObject(Seq(
      "type" -> JsString("variable"),
      "symbol" -> JsString(symbol)
    ))
    case Not(e) => JsObject(Seq(
      "type" -> JsString("not"),
      "e" -> buildJson(e)
    ))
    case Or(e1, e2) => JsObject(Seq(
      "type" -> JsString("or"),
      "e1" -> buildJson(e1),
      "e2" -> buildJson(e2)
    ))
    case And(e1, e2) => JsObject(Seq(
      "type" -> JsString("and"),
      "e1" -> buildJson(e1),
      "e2" -> buildJson(e2)
    ))
  }
}