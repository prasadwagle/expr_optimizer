expr_optimizer
==============

Determines pre-filters for boolean expressions

Pseudo-code:

<pre>
<code>
abstract class Expr
case class Lit(value: Boolean) extends Expr
case class Var(name: String) extends Expr
case class Not(arg: Expr) extends Expr
case class And(left: Expr, right: Expr) extends Expr
case class Or(left: Expr, right: Expr) extends Expr

type CNF = List[Expr]

val expr = ExprParser(input)
val removeLiteralsExpr = removeLiterals(expr)
val negationsInExpr = negationsIn(removeLiteralsExpr)
val orDistributedOverAndExpr = orDistributedOverAnd(negationsInExpr)
val cnf = convertExprCNFToCNF(orDistributedOverAndExpr)
val cheapCnf = cnf.filter(expr => cheapExpr(expr))
val preFilterExpr = cnfToExpr(cheapCnf)
val output = printExpr(prevalExpr)
</code>
</pre>
