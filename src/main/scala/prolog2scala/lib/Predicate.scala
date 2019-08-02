package prolog2scala.lib

import prolog2scala.lib.Clause.Solution.{CutSolution, SimpleSolution}
import prolog2scala.lib.Implicits._

case class Predicate[-A,+B](clauses: Clause[A,B]*) {
  def apply(arg: A): LazyList[B] =
    clauses to LazyList map {
      _(arg)
    } takeUntil {
      case CutSolution(_) => true
      case _ => false
    } flatMap (_ solutionsLazyList)
}

