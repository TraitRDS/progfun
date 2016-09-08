package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (key, value) => (key, Signal(eval(value(), namedExpressions)))}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalHelp(expr: Expr, references: Map[String, Signal[Expr]], backReferences: Map[Expr, Set[Expr]]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) => {
          val refExpr = getReferenceExpr(name, references)
          val backRefs = backReferences.withDefaultValue(Set[Expr]())(refExpr)

          if (backRefs.contains(expr)) {
            //throw new Error("cyclic reference")
            Double.NaN
          } else {
            evalHelp(refExpr, references, getBackReferencesMap(backReferences, (refExpr, expr)))
          }
        }
        case Plus(a, b) => binaryOpEval(expr, a, b, backReferences)(_+_)
        case Minus(a, b) => binaryOpEval(expr, a, b, backReferences)(_-_)
        case Times(a, b) => binaryOpEval(expr, a, b, backReferences)(_*_)
        case Divide(a, b) => binaryOpEval(expr, a, b, backReferences)(_/_)
      }
    }
    def binaryOpEval(expr: Expr, a: Expr, b: Expr, backReferences: Map[Expr, Set[Expr]])
                    (binaryOp: (Double, Double) => Double): Double = {
      val backRef: Set[Expr] = backReferences.withDefaultValue(Set[Expr]())(expr)
      if (backRef.contains(a)) {
        //throw new Error("cyclic reference.")
        Double.NaN
      }
      if (backRef.contains(b)) {
        //throw new Error("cyclic reference.")
        Double.NaN
      }
      val newBackReferences = getBackReferencesMap(backReferences, (a, expr), (b, expr))
      binaryOp(evalHelp(a, references, newBackReferences), evalHelp(b, references, newBackReferences))
    }

    evalHelp(expr, references, Map[Expr, Set[Expr]]())
  }


  private def getBackReferencesMap(backReferences: Map[Expr, Set[Expr]], newBackRefs: (Expr,Expr)*) = {
    val list = newBackRefs.toList

    def getBackReferencesMapList(backReferences: Map[Expr, Set[Expr]], list: List[(Expr,Expr)]):  Map[Expr, Set[Expr]]= list match {
      case Nil => backReferences
      case (expr, referencedExpr)::tail => {
        val backRefs = Set(referencedExpr) ++ backReferences.withDefaultValue(Set[Expr]())(referencedExpr)
        getBackReferencesMapList(backReferences.updated(expr, backRefs), tail)
      }
    }
    getBackReferencesMapList(backReferences, list)
  }
  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
