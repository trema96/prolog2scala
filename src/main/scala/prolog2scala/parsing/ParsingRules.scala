package prolog2scala.parsing

import fastparse._
import StringParser._
import TermParser._
import PrologWhitespace._

object ParsingRules {
  def program[_: P]: P[Program] = P(Start ~ translationDirective.rep(1) ~ clause.rep(1) ~ End) map {
    case (directives, clauses) => Program(directives,
      clauses.groupBy(clause => (clause.head.name, clause.head.args.length))
    )
  }
  def clause[_: P]: P[Clause] = P(struct ~ (":-" ~ termList).? ~ ".") map {
    case (head, body) => Clause(head, body.getOrElse(Seq.empty))
  }
  def translationDirective[_: P]: P[TranslationDirective] = P("#" ~ name ~ ":" ~ name ~~ "(" ~ (("+" | "-").! ~ name).rep(sep=",", min=1) ~ ")") map {
    case (scalaName, predName, args) =>
      TranslationDirective(scalaName, predName, args map {
        case ("+", name) => PredicateArgument(name, PredicateArgument.Type.In)
        case ("-", name) => PredicateArgument(name, PredicateArgument.Type.Out)
      })
  }
}