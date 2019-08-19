package prolog2scala.translation

import fastparse._
import org.scalatest.FlatSpec
import PredicateArgument.Direction.{In, Out}
import prolog2scala.translation.Term.{ListTerm, Struct, Variable}
import prolog2scala.translation.parsing.ParsingRules

class TestParsing extends FlatSpec {
  "A correct program" should "be correctly parsed" in {
    val program =
      """
        |#lookup: lookup(+list, -elem, -position, -listNoElem)
        |lookup([H|T],H,zero,T).
        |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    assert {
      parseResult == Program(
        Seq(TranslationDirective("lookup", "lookup", Seq(
          PredicateArgument("list", In),
          PredicateArgument("elem", Out),
          PredicateArgument("position", Out),
          PredicateArgument("listNoElem", Out)
        ))),
        Map(
          ("lookup", 4) -> Seq(
            Clause(
              Struct("lookup", Seq(
                ListTerm(Seq(Variable("H")), Some(Variable("T"))),
                Variable("H"),
                Struct("zero", Seq()),
                Variable("T")
              )),
              Seq()
            ),
            Clause(
              Struct("lookup", Seq(
                ListTerm(Seq(Variable("H")), Some(Variable("T"))),
                Variable("E"),
                Struct("s", Seq(Variable("N"))),
                ListTerm(Seq(Variable("H")), Some(Variable("T2")))
              )),
              Seq(
                Struct("lookup", Seq(
                  Variable("T"),
                  Variable("E"),
                  Variable("N"),
                  Variable("T2")
                ))
              )
            )
          )
        )
      )
    }
  }

  "Another correct program" should "be parsed" in {
    val program =
      """
        |#permutation: permutation(+list, -permutations)
        |member2([X|Xs],X,Xs).
        |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
        |permutation([],[]).
        |permutation(Xs,[X|Ys]) :-
        | member2(Xs,X,Zs),
        | permutation(Zs, Ys).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    //TODO assert == expected
  }
}
