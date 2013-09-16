/**
 * Created with IntelliJ IDEA.
 * User: pwagle
 * Date: 9/15/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */
import scala.util.parsing.combinator._

//formula     := variable | literal | expression
//variable    := cheap | expensive
//cheap       := v[0-9]+
//expensive   := w[0-9]+
//literal     := "T" | "F"
//expression  := conjunction | disjunction | negation
//conjunction := "(and" ws formula ws formula ")"
//disjunction := "(or" ws formula ws formula ")"
//negation    := "(not" ws formula ")"
//ws          := " "+

object ExprParser extends RegexParsers {
  def formula: Parser[Expr] = variable | literal | expression
  def variable: Parser[Var] = cheap | expensive
  def cheap: Parser[Var] = """v[0-9]+""".r ^^ (x => Var(x))
  def expensive: Parser[Var] = """w[0-9]+""".r ^^ (x => Var(x))
  def literal: Parser[Lit] = "T" ^^ (x => Lit(true)) | "F" ^^ (x => Lit(false))
  def expression: Parser[Expr] = conjunction | disjunction | negation
  def conjunction: Parser[And] = "(and" ~ formula ~ formula ~ ")" ^^
    { case "(and" ~ f1 ~ f2 ~ ")" => And(f1, f2) }
  def disjunction: Parser[Or] = "(or" ~ formula ~ formula ~ ")" ^^
    { case "(or" ~ f1 ~ f2 ~ ")" => Or(f1, f2) }
  def negation: Parser[Not] = "(not" ~ formula ~ ")" ^^
    { case "(not" ~ f1 ~ ")" => Not(f1) }
  def apply(input: String): Option[Expr] = parseAll(formula, input) match {
    case Success(result, _) => Some(result)
    case NoSuccess(_, _) => None
  }
}