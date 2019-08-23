package prolog2scala.lib

import org.scalatest.{FlatSpec, Matchers}
import prolog2scala.lib.Clause.{Fact, Rule}
import prolog2scala.lib.Cut._

class TestLib extends FlatSpec with Matchers {
  "Lookup not using cut" should "return multiple result, each removing a different occurrence of the element matching the provided one" in {
    def lookup[A](list: List[A], elem: A): Stream[List[A]] = Predicate[(List[A], A), List[A]](
      Fact{case (h :: t, h1) if h == h1 => t},
      Rule{case (h :: t, e) => for (t2 <- lookup(t, e)) yield h +: t2}
    )((list, elem))

    (lookup(List(1,7,7,2,7,3), 7) toList) shouldEqual List(List(1,7,2,7,3), List(1,7,2,7,3), List(1,7,7,2,3))
  }

  "Lookup using cut" should "return a single result, removing the first occurrence of the element matching the provided one" in {
    def lookupCut[A](list: List[A], elem: A): Stream[List[A]] = Predicate[(List[A], A), List[A]](
      Rule.withCut{case (h :: t, h1) if h == h1 => ! Stream(t)},
      Rule{case (h :: t, e) => for (t2 <- lookupCut(t, e)) yield h +: t2}
    )((list, elem))

    (lookupCut(List(1,7,7,2,7,3), 7) toList) shouldEqual List(List(1,7,2,7,3))
  }

  //A solution that was cut should not be explored more than what is needed to determine if the cut predicate was reached
  "Cut" should "be lazy" in {
    var evaluatedVariables: List[Int] = Nil

    //range(X,_,X).
    //range(X,Y,Z) :- X < Y, X2 is X + 1, range(X2, Y, Z).
    def range(from: Int, to: Int): Stream[Int] = Predicate[(Int, Int), Int](
      Fact{case (x, _) => evaluatedVariables :+= x; x },
      Rule{case (x, y) if x < y => range(x+1, y)}
    )(from, to)
    //example(X, Y) :- range(1,3,X), !, range(1,3,Y).
    def example(): Stream[(Int, Int)] = Predicate[Unit, (Int, Int)](
      Rule.withCut{case _ => range(1, 3) ! (x => for (y <- range(1,3)) yield (x,y))}
    )()

    (example() toList) shouldEqual List((1,1),(1,2),(1,3))
    evaluatedVariables shouldEqual List(1,1,2,3)
  }

  "Cut" should "cut only if evaluated" in {
    //isLessThan3(X) :- X < 3.
    def isLessThan3(value: Int): Stream[Unit] = Predicate[Int, Unit](
      Fact{case n if n < 3 => ()}
    )(value)

    //incOrDec(X, Y) :- isLessThan3(X), !, Y is X - 1.
    //incOrDec(X, Y) :- Y is X + 1.
    def incOrDec(value: Int): Stream[Int] = Predicate[Int, Int](
      Rule.withCut{case x => isLessThan3(x) ! (_ => Stream(x - 1))},
      Fact{case x => x+1}
    )(value)

    (incOrDec(2) toList) shouldEqual List(1)
    (incOrDec(3) toList) shouldEqual List(4)
  }
}
