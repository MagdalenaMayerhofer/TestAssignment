object AlgebraicTransformation {

  /***
    * This function simplifies a given BooleanExpression
    * @param booleanExpression: the BooleanExpression to be simplified
    * @return the simplified BooleanExpression
    */
  def simplifyExpression(booleanExpression: BooleanExpression): BooleanExpression = {
    booleanExpression match {
      case True => True
      case False => False
      case Variable(symbol) => Variable(symbol)
      case Not(True) => False
      case Not(False) => True
      case Not(Not(e)) => simplifyExpression(e)
      case Not(And(e1, e2)) => simplifyExpression(Or(Not(e1), Not(e2)))
      case Not(Or(e1, e2)) => simplifyExpression(And(Not(e1), Not(e2)))
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

  /***
    * This function creates a CNF out of an BooleanExpression.
    * To create the CNF the Tseitin Encoding is used
    * @param booleanExpression: the BooleanExpression you want to get a CNF from
    * @return the created CNF
    */
  def createCnf(booleanExpression: BooleanExpression): BooleanExpression = {
    var partOfCnf = createCnfHelper(booleanExpression)
    if (booleanExpression != partOfCnf) {
      partOfCnf = And(booleanExpression, partOfCnf)
    }
    partOfCnf
  }

  def createCnfHelper(booleanExpression: BooleanExpression): BooleanExpression = booleanExpression match {
    case Or(e1, e2) =>
      var result = And(And(Or(Not(e1), booleanExpression), Or(Not(e2), booleanExpression)), Or(Or(Not(booleanExpression), e1), e2))
      val e1Cnf = createCnfHelper(e1)
      val e2Cnf = createCnfHelper(e2)
      if (e1 != e1Cnf) {
        result = And(result, e1Cnf)
      }
      if (e2 != e2Cnf) {
        result = And(result, e2Cnf)
      }
      result
    case And(e1, e2) =>
      var result = And(And(Or(Not(booleanExpression), e1), Or(Not(booleanExpression), e2)), Or(Or(Not(e1), Not(e2)), booleanExpression))
      val e1Cnf = createCnfHelper(e1)
      val e2Cnf = createCnfHelper(e2)
      if (e1 != e1Cnf) {
        result = And(result, e1Cnf)
      }
      if (e2 != e2Cnf) {
        result = And(result, e2Cnf)
      }
      result
    case Not(e) =>
      var result = And(Or(Not(booleanExpression), Not(e)), Or(booleanExpression, e))
      val eCnf = createCnfHelper(e)
      if (e != eCnf) {
        result = And(result, eCnf)
      }
      result
    case _ => booleanExpression
  }
}
