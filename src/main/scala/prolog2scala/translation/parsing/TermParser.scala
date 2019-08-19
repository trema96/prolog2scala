package prolog2scala.translation.parsing

import fastparse._
import prolog2scala.translation.Term
import prolog2scala.translation.Term._
import prolog2scala.translation.parsing.OperationParser._
import prolog2scala.translation.parsing.PrologWhitespace._
import prolog2scala.translation.parsing.StringParser._

object TermParser {
  def termList[_: P]: P[Seq[Term]] = P(term.rep(min = 1, sep = ","))
  def termNoOp[_: P]: P[Term] = P(
    struct |
    floatTerm |
    intTerm |
    cut |
    list |
    varTerm
  )
  def term[_: P]: P[Term] = P(termNoOp | operation)
  def struct[_: P]: P[Struct] = P(name ~~ ("(" ~ termList ~ ")").?) map {
    case (name, args) => Struct(name, args.getOrElse(Seq.empty))
  }
  def varTerm[_: P]: P[Variable] = variableName map Variable
  def intTerm[_: P]: P[NumberTerm.Int] = intString map (x => NumberTerm.Int(x.toInt))
  def floatTerm[_: P]: P[NumberTerm.Float] = floatString map (x => NumberTerm.Float(x.toDouble))
  def cut[_: P]: P[Term] = P("!") map (_ => Cut)
  def list[_: P]: P[ListTerm] = P("[" ~ (termList ~ ("|" ~ varTerm).?).? ~ "]") map {
    case Some((values, tail)) => ListTerm(values, tail)
    case None => ListTerm(Seq.empty, None)
  }
}
