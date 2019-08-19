package prolog2scala.translation.parsing

import fastparse._
import prolog2scala.translation.parsing.PrologWhitespace._
import prolog2scala.translation.parsing.StringParser._
import prolog2scala.translation.parsing.TermParser._
import prolog2scala.translation._

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
        case ("+", name) => PredicateArgument(name, PredicateArgument.Direction.In)
        case ("-", name) => PredicateArgument(name, PredicateArgument.Direction.Out)
      })
  }
}