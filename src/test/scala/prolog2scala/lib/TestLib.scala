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
    def lookup[A](list: List[A], elem: A): Stream[List[A]] = Predicate[(List[A], A), List[A]](
      Rule.withCut{case (h :: t, h1) if h == h1 => ! Stream(t)},
      Rule{case (h :: t, e) => for (t2 <- lookup(t, e)) yield h +: t2}
    )((list, elem))

    (lookup(List(1,7,7,2,7,3), 7) toList) shouldEqual List(List(1,7,2,7,3))
  }
}
