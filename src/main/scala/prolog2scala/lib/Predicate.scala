package prolog2scala.lib

import prolog2scala.lib.Clause.Solution.{CutSolution, SimpleSolution}
import Implicits._

case class Predicate[-A,+B](clauses: Clause[A,B]*) {
  def apply(arg: A): Stream[B] =
    (clauses toStream).iterator map {
      _(arg)
    } takeTo {
      case CutSolution(_) => true
      case _ => false
    } flatMap (_ solutionsStream) toStream
}

