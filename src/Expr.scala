/**
 * Created with IntelliJ IDEA.
 * User: pwagle
 * Date: 9/14/13
 * Time: 8:31 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class Expr
case class Lit(value: Boolean) extends Expr
case class Var(name: String) extends Expr
case class Not(arg: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr

object ExprOptimizer {

  type CNF = List[Expr]
  def CNF(exprs: Expr*) = List(exprs: _*)

  def exprStrToPrevalStr(input: String): String = {
    //println(input)
    val exprOption = ExprParser(input)
    val expr = exprOption match {
      case Some(e) => e
      case None => return "Error"
    }
    //println(expr)
    val cnf = prevalidatorCNF(expr)
    //println(cnf)
    if (cnf.isEmpty) {
      return ""
    } else {
      val prevalExpr = cnfToExpr(cnf)
      return printExpr(prevalExpr)
    }
  }

  def printExpr(expr: Expr): String = {
    expr match {
      case Lit(true) => "T"
      case Lit(false) => "F"
      case Not(expr1) => "(not " + printExpr(expr1) + ")"
      case And(expr1, expr2) => "(and " + printExpr(expr1) + " " + printExpr(expr2) + ")"
      case Or(expr1, expr2) => "(or " + printExpr(expr1) + " " + printExpr(expr2) + ")"
      case Var(v) => v
    }
  }

  def cnfToExpr(cnf: CNF): Expr = {
    cnf.reduceLeft((x,y) => And(x, y))
  }

  def prevalidatorCNF(expr: Expr): CNF = {
    val cnf = convertToCNF(expr)
    val cheapCNF = cnf.filter(expr => cheapExpr(expr))
    return cheapCNF
  }

  def cheapExpr(expr: Expr) = {
    variables(expr).forall(v => v.startsWith("v"))
  }

  def convertToCNF(expr: Expr): CNF = {
    val removeLiteralsExpr = removeLiterals(expr)
    println(removeLiteralsExpr)
    val negationsInExpr = negationsIn(removeLiteralsExpr)
    println(negationsInExpr)
    val orDistributedOverAndExpr = orDistributedOverAnd(negationsInExpr)
    println(orDistributedOverAndExpr)
    val cnf = convertExprCNFToCNF(orDistributedOverAndExpr)
    println(cnf)
    return cnf
  }

  def convertExprCNFToCNF(exprCNF: Expr): CNF = {
    exprCNF match {
      case And(expr1, expr2) => convertExprCNFToCNF(expr1) ::: convertExprCNFToCNF(expr2)
      case _ => CNF(exprCNF)
    }
  }

  // expr has no literals
  def negationsIn(expr: Expr): Expr = {
    expr match {
      case Not(Not(expr1)) => expr1
      case Not(And(expr1, expr2)) => Or(negationsIn(Not(expr1)), negationsIn(Not(expr2)))
      case Not(Or(expr1, expr2)) => And(negationsIn(Not(expr1)), negationsIn(Not(expr2)))
      case And(expr1, expr2) => And(negationsIn(expr1), negationsIn(expr2))
      case Or(expr1, expr2) => Or(negationsIn(expr1), negationsIn(expr2))
      case _ => expr
    }
  }

  def orDistributedOverAnd(expr: Expr): Expr = {
    expr match {
      case And(expr1, expr2) => And(orDistributedOverAnd(expr1), orDistributedOverAnd(expr2))
      case Or(expr1, And(expr2, expr3)) => And(orDistributedOverAnd(Or(expr1, expr2)),
        orDistributedOverAnd(Or(expr1, expr3)))
      case Or(And(expr1, expr2), expr3) => And(orDistributedOverAnd(Or(expr1, expr3)),
        orDistributedOverAnd(Or(expr2, expr3)))
      case _ => expr
    }
  }

  def containsLiterals(expr: Expr): Boolean = {
    expr match {
      case Lit(_) => true
      case Not(expr1) => containsLiterals(expr1)
      case And(expr1, expr2) => containsLiterals(expr1) || containsLiterals(expr2)
      case Or(expr1, expr2) => containsLiterals(expr1) || containsLiterals(expr2)
      case Var(_) => false
    }
  }

  def variables(expr: Expr): List[String] = {
    expr match {
      case Not(expr1) => variables(expr1)
      case And(expr1, expr2) => variables(expr1) ::: variables(expr2)
      case Or(expr1, expr2) => variables(expr1) ::: variables(expr2)
      case Var(v) => List(v)
      case _ => List()
    }
  }

  def removeLiterals(expr: Expr): Expr = {
    val c = containsLiterals(expr)
    if (!c)
      return expr
    expr match {
      case Not(Lit(true)) => Lit(false)
      case Not(Lit(false)) => Lit(true)
      case Not(expr1) => removeLiterals(Not(removeLiterals(expr1)))
      case And(Lit(x), Lit(y)) => Lit(x && y)
      case And(Lit(true), expr1) => removeLiterals(expr1)
      case And(expr1, Lit(true)) => removeLiterals(expr1)
      case And(Lit(false), expr1) => Lit(false)
      case And(expr1, Lit(false)) => Lit(false)
      case And(expr1, expr2) => removeLiterals(And(removeLiterals(expr1), removeLiterals(expr2)))
      case Or(Lit(x), Lit(y)) => Lit(x || y)
      case Or(Lit(true), expr1) => Lit(true)
      case Or(expr1, Lit(true)) => Lit(true)
      case Or(Lit(false), expr1) => removeLiterals(expr1)
      case Or(expr1, Lit(false)) => removeLiterals(expr1)
      case Or(expr1, expr2) => removeLiterals(Or(removeLiterals(expr1), removeLiterals(expr2)))
      case _ => expr
    }
  }

  def main(args: Array[String]) {
    for( ln <- io.Source.stdin.getLines ) {
      println("Input: " + ln)
      println("Output: " + exprStrToPrevalStr(ln) )
    }
  }
}