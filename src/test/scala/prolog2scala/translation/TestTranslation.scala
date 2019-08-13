package prolog2scala.translation

import fastparse.{Parsed, parse}
import org.scalatest.{FlatSpec, Matchers}
import prolog2scala.translation.parsing.ParsingRules

/*
class TestTranslation extends FlatSpec with Matchers {
  "A correct program" should "correctly translate" in {
    val program =
      """
        |#lookup: lookup(+list, -elem, -position, -listNoElem)
        |lookup([H|T],H,zero,T).
        |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    parseResult.translate() should matchPattern {
      case PredicateTranslationResult.Success(_) =>
    }
  }

  "A correct program, with invalid input types" should "not correctly translate" in {
    val program =
      """
        |#lookup: lookup(-list, +elem, +position, -listNoElem)
        |lookup([H|T],H,zero,T).
        |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    parseResult.translate() should matchPattern {
      case OldTranslationResult.Failure(_) =>
    }
  }

  "A program with more than 1 predicate" should "translate as well" in {
    val program =
      """
        |#permutation: permutation(+list, -permutations)
        |member2([X|Xs],X,Xs).
        |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
        |permutation([],[]).
        |permutation(Xs, [X | Ys]) :-
        | member2(Xs,X,Zs),
        | permutation(Zs, Ys).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    parseResult.translate() should matchPattern {
      case PredicateTranslationResult.Success(_) =>
    }

  }

  "A program with more than 1 predicate and 2 directives" should "translate as well" in {
    val program =
      """
        |#permutation: permutation(+list, -permutations)
        |#member2: member2(+list,+elem,-listNoElem)
        |member2([X|Xs],X,Xs).
        |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
        |permutation([],[]).
        |permutation(Xs, [X | Ys]) :-
        | member2(Xs,X,Zs),
        | permutation(Zs, Ys).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    parseResult.translate() should matchPattern {
      case PredicateTranslationResult.Success(_) =>
    }
  }

  "A program with more than 1 predicate" should "not translate if the directive makes the other predicate invalid" in {
    val program =
      """
        |#permutation: permutation(-list, +permutations)
        |member2([X|Xs],X,Xs).
        |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
        |permutation([],[]).
        |permutation(Xs, [X | Ys]) :-
        | member2(Xs,X,Zs),
        | permutation(Zs, Ys).
      """.stripMargin
    val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
    parseResult.translate() should matchPattern {
      case OldTranslationResult.Failure(_) =>
    }
  }
}

*/