import com.fasterxml.jackson.core.JsonParseException
import play.api.libs.json._

object Parser {

  def parseJsonToBooleanExpression(json: String): BooleanExpression = {
    try {
      val jsValue = Json.parse(json)
      buildBooleanExpression(jsValue)
    }
    catch {
      case _: JsonParseException => throw JsonParseErrorException("Not a valid JSON format!")
    }
  }

  def parseBooleanExpressionToJson(booleanExpression: BooleanExpression): String = {
    buildJson(booleanExpression).toString()
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

  def simplifyExpression(booleanExpression: BooleanExpression): BooleanExpression = {
    print(booleanExpression + "\n")
    booleanExpression match {
      case True => True
      case False => False
      case Variable(symbol) => Variable(symbol)
      case Not(True) => False
      case Not(False) => True
      case Not(Not(e)) => simplifyExpression(e)
      case Not(e) =>
        val e1 = simplifyExpression(e)
        if (e1 != e) {
          simplifyExpression(Not(e1))
        }
        else {
          Not(e)
        }
      case Or(True, e2) => True
      case Or(e1, True) => True
      case Or(False, e2) => simplifyExpression(e2)
      case Or(e1, False) => simplifyExpression(e1)
      case Or(e1, e2) =>
        if (e1 == e2) {
          return simplifyExpression(e1)
        }
        if (e1 == Not(e2) || e2 == Not(e1)) {
          return False
        }
        val e3 = simplifyExpression(e1)
        val e4 = simplifyExpression(e2)
        if (e1 != e3 || e2 != e4) {
          simplifyExpression(Or(e3, e4))
        }
        else {
          Or(e1, e2)
        }
      case And(True, e2) => simplifyExpression(e2)
      case And(e1, True) => simplifyExpression(e1)
      case And(False, e2) => False
      case And(e1, False) => False
      case And(e1, e2) =>
        if (e1 == e2) {
          return simplifyExpression(e1)
        }
        if (e1 == Not(e2) || e2 == Not(e1)) {
          return False
        }
        val e3 = simplifyExpression(e1)
        val e4 = simplifyExpression(e2)
        if (e1 != e3 || e2 != e4) {
          simplifyExpression(And(e3, e4))
        }
        else {
          And(e1, e2)
        }
    }
  }
}