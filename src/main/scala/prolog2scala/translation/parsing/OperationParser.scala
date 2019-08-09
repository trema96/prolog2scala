package prolog2scala.translation.parsing

import fastparse._
import prolog2scala.translation.Term
import prolog2scala.translation.Term.Struct
import prolog2scala.translation.parsing.PrologWhitespace._
import prolog2scala.translation.parsing.TermParser._

object OperationParser {
  //TODO riprovare a generalizzare
  def operation[_: P]: P[Term] = compare

  private def operand[_: P]: P[Term] = P(termNoOp | ("(" ~ compare ~ ")"))
  private def mul[_: P]: P[Term] = P(operand ~ (StringIn("*","/","%").! ~ operand).rep) map opToTerm
  private def sum[_: P]: P[Term] = P(mul ~ (StringIn("+","-").! ~ mul).rep) map opToTerm
  private def compare[_: P]: P[Term] = P(sum ~ (StringIn(">",">=","<","=<","is", "=").! ~ sum).rep) map opToTerm

  private def opToTerm: PartialFunction[(Term, Seq[(String, Term)]), Term] = {
    case (left, Nil) => left
    case (left, (op, right) :: t) => opToTerm(Struct(op, Seq(left, right)), t)
  }
}
