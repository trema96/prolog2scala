package prolog2scala.lib

import prolog2scala.lib.Clause.Solution.{CutSolution, SimpleSolution}
import prolog2scala.lib.Clause.Solution

sealed trait Clause[-A,+B] {
  def apply(arg: A): Solution[B]
}

object Clause {
  sealed trait Solution[+A] {
    def solutionsLazyList: LazyList[A]
  }

  object Solution {
    case class SimpleSolution[+A](override val solutionsLazyList: LazyList[A]) extends Solution[A]
    case class CutSolution[+A](override val solutionsLazyList: LazyList[A]) extends Solution[A]
  }

  class Rule[-A,+B] private (body: PartialFunction[A, Solution[B]]) extends Clause[A,B] {
    override def apply(arg: A): Solution[B] = body.applyOrElse(arg, (_: A) => SimpleSolution(LazyList()))
  }

  object Rule {
    def apply[A,B](body: PartialFunction[A, LazyList[B]]): Rule[A,B] = new Rule(body.andThen(SimpleSolution(_)))
    def withCut[A,B](body: PartialFunction[A, Cut.CutResult[B]]): Rule[A,B] =
      new Rule(body.andThen(res => if (res.didCut) CutSolution(res.solutions) else SimpleSolution(res.solutions)))
  }

  case class Fact[-A, +B](body: PartialFunction[A, B]) extends Clause[A,B] {
    override def apply(arg: A): Solution[B] = SimpleSolution(body.andThen(LazyList(_)).applyOrElse(arg, (_: A) => LazyList()))
  }
}
