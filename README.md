## Test Assignment

#### Magdalena Mayerhofer

##### What I want to do

I find the giter8 templates integration and the standalone IntelliJ inspection 
the most interesting.

##### Warmup

You find the Warmup assignment in the following file: 
* /src/main/scala/recursiveFunction.sc

##### To my JSON serialization Project:

In /src/main/scala you find the following files:
* AST.scala: There you find the AST of the BooleanExpression
* JsonParseErrorException.scala: There you find the Exception what gets 
thrown when parsing errors occur
* Parser.scala: Here you find the methods to parse a JSON string to a 
BooleanExpression and a BooleanExpression to a JSON string:
    * parseJsonToBooleanExpression(json: String): BooleanExpression
    * buildBooleanExpression(json: JsValue): BooleanExpression
    * parseBooleanExpressionToJson(booleanExpression: BooleanExpression): String
    * buildJson(booleanExpression: BooleanExpression): JsObject = booleanExpression
* AlgebraicTransformation.scala: Her you find the methods to simplify a 
BooleanExpression and to create a CNF out of a given BooleanExpression
    * simplifyExpression(booleanExpression: BooleanExpression): BooleanExpression
    * createCnf(booleanExpression: BooleanExpression): BooleanExpression
    * createCnfHelper(booleanExpression: BooleanExpression): BooleanExpression

You find the Testcases in /src/test/scala/TestCases.scala.