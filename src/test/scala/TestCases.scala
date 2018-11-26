import org.scalatest.FunSuite
import org.scalatest.Matchers._
import play.api.libs.json.{JsObject, JsString}

class TestCases extends FunSuite{

  //Minimal test cases with correct input

  test ("Minimal: True") {
    val json: String = JsObject(Seq(
      "type" -> JsString("true")
    )).toString()
    val booleanExpression: BooleanExpression = True
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("Minimal: False") {
    val json: String = JsObject(Seq(
      "type" -> JsString("false")
    )).toString()
    val booleanExpression: BooleanExpression = False
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("Minimal: Variable") {
    val json: String = JsObject(Seq(
      "type" -> JsString("variable"),
      "symbol" -> JsString("a")
    )).toString()
    val booleanExpression: BooleanExpression = Variable("a")
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("Minimal: Not") {
    val json: String = JsObject(Seq(
      "type" -> JsString("not"),
      "e" -> JsObject(Seq(
        "type" -> JsString("true")
      ))
    )).toString()
    val booleanExpression: BooleanExpression = Not(True)
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("Minimal: Or") {
    val json: String = JsObject(Seq(
      "type" -> JsString("or"),
      "e1" -> JsObject(Seq(
        "type" -> JsString("false")
      )),
      "e2" -> JsObject(Seq(
        "type" -> JsString("variable"),
        "symbol" -> JsString("b")
      ))
    )).toString()
    val booleanExpression: BooleanExpression = Or(False, Variable("b"))
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("Minimal: And") {
    val json: String = JsObject(Seq(
      "type" -> JsString("and"),
      "e1" -> JsObject(Seq(
        "type" -> JsString("true")
      )),
      "e2" -> JsObject(Seq(
        "type" -> JsString("false")
      ))
    )).toString()
    val booleanExpression: BooleanExpression = And(True, False)
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  //More complex test cases with correct input

  test ("TC 1") {
    val json: String = JsObject(Seq(
      "type" -> JsString("not"),
      "e" -> JsObject(Seq(
        "type" -> JsString("and"),
        "e1" -> JsObject(Seq(
          "type" -> JsString("and"),
          "e1" -> JsObject(Seq(
            "type" -> JsString("or"),
            "e1" -> JsObject(Seq(
              "type" -> JsString("variable"),
              "symbol" -> JsString("a")
            )),
            "e2" -> JsObject(Seq(
              "type" -> JsString("variable"),
              "symbol" -> JsString("b")
            ))
          )),
          "e2" -> JsObject(Seq(
            "type" -> JsString("true")
          ))
        )),
        "e2" -> JsObject(Seq(
          "type" -> JsString("false")
        ))
      ))
    )).toString()

    val booleanExpression: BooleanExpression = Not(And(And(Or(Variable("a"), Variable("b")), True), False))
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  test ("TC 2") {
    val json: String = JsObject(Seq(
      "type" -> JsString("not"),
      "e" -> JsObject(Seq(
        "type" -> JsString("or"),
        "e1" -> JsObject(Seq(
          "type" -> JsString("not"),
          "e" -> JsObject(Seq(
              "type" -> JsString("variable"),
              "symbol" -> JsString("a")
          ))
        )),
        "e2" -> JsObject(Seq(
          "type" -> JsString("and"),
          "e1" -> JsObject(Seq(
            "type" -> JsString("variable"),
            "symbol" -> JsString("c")
          )),
          "e2" -> JsObject(Seq(
            "type" -> JsString("variable"),
            "symbol" -> JsString("someVar")
          ))
        ))
      ))
    )).toString()

    val booleanExpression: BooleanExpression = Not(Or(Not(Variable("a")), And(Variable("c"), Variable("someVar"))))
    assert(booleanExpression == Parser.parseJsonToBooleanExpression(json))
    assert(json == Parser.parseBooleanExpressionToJson(booleanExpression))
  }

  //Test cases with faulty/wrong input

  test ("No JSON") {
    val someString: String = "I am just a random String"
    an [JsonParseErrorException] should be thrownBy Parser.parseJsonToBooleanExpression(someString)
  }

  test ("No valid boolean expression type") {
    val json: String = JsObject(Seq(
      "type" -> JsString("wrongType"),
      "symbol" -> JsString("a")
    )).toString()
    an [JsonParseErrorException] should be thrownBy Parser.parseJsonToBooleanExpression(json)
  }

  test ("Not all parameters in JSON") {
    val json: String = JsObject(Seq(
      "type" -> JsString("and"),
      "e1" -> JsObject(Seq(
        "type" -> JsString("true")
      ))
    )).toString()
    an [JsonParseErrorException] should be thrownBy Parser.parseJsonToBooleanExpression(json)
  }

  test ("No type parameter in JSON") {
    val json: String = JsObject(Seq(
      "e1" -> JsObject(Seq(
        "type" -> JsString("false")
      )),
      "e2" -> JsObject(Seq(
        "type" -> JsString("variable"),
        "symbol" -> JsString("b")
      ))
    )).toString()
    an [JsonParseErrorException] should be thrownBy Parser.parseJsonToBooleanExpression(json)
  }

  //some simplification test cases

  test ("simplify 1") {
    val booleanExpression: BooleanExpression = Not(Not(True))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == True)
  }

  test ("simplify 2") {
    val booleanExpression: BooleanExpression = And(Variable("a"), True)
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == Variable("a"))
  }

  test ("simplify 3") {
    val booleanExpression: BooleanExpression = And(Or(True, Variable("c")), Variable("b"))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == Variable("b"))
  }

  test ("simplify 4") {
    val booleanExpression: BooleanExpression = Not(Or(And(True, Variable("x")), Not(False)))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == False)
  }

  test ("simplify 5") {
    val booleanExpression: BooleanExpression = And(Not(Or(Variable("a"), False)), And(Variable("a"), True))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == False)
  }

  test ("simplify 6") {
    val booleanExpression: BooleanExpression = Not(Or(Variable("a"), Variable("b")))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == And(Not(Variable("a")), Not(Variable("b"))))
  }

  test ("simplify 7") {
    val booleanExpression: BooleanExpression = Not(And(Not(Variable("a")), Variable("b")))
    assert(AlgebraicTransformation.simplifyExpression(booleanExpression) == Or(Variable("a"), Not(Variable("b"))))
  }

  // CNF test case

  test ("cnf") {
    val booleanExpression: BooleanExpression = Not(And(Or(Variable("a"), Variable("c")), Variable("d")))
    val cnf: BooleanExpression = And(Not(And(Or(Variable("a"), Variable("c")), Variable("d"))),
      And(And(Or(Not(Not(And(Or(Variable("a"),Variable("c")),Variable("d")))),Not(And(Or(Variable("a"),
      Variable("c")),Variable("d")))),Or(Not(And(Or(Variable("a"),Variable("c")),Variable("d"))),
      And(Or(Variable("a"),Variable("c")),Variable("d")))),And(And(And(Or(Not(And(Or(Variable("a"),Variable("c")),
      Variable("d"))),Or(Variable("a"),Variable("c"))),Or(Not(And(Or(Variable("a"),Variable("c")),Variable("d"))),
      Variable("d"))),Or(Or(Not(Or(Variable("a"),Variable("c"))),Not(Variable("d"))),And(Or(Variable("a"),
      Variable("c")),Variable("d")))),And(And(Or(Not(Variable("a")),Or(Variable("a"),Variable("c"))),
      Or(Not(Variable("c")),Or(Variable("a"),Variable("c")))),Or(Or(Not(Or(Variable("a"),Variable("c"))),
      Variable("a")),Variable("c"))))))
    assert(AlgebraicTransformation.createCnf(booleanExpression) == cnf)
  }
}
