package prolog2scala.lib

import prolog2scala.lib.Clause.Solution.{CutSolution, SimpleSolution}
import Implicits._

/**
  * Represents a prolog predicate
  * @param clauses predicate clauses
  * @tparam A predicate inputs
  * @tparam B predicate outputs
  */
case class Predicate[-A,+B](clauses: Clause[A,B]*) {
  /**
    * Get solutions of this predicate
    * @param arg the predicate arguments inputs
    * @return the predicate solutions
    */
  def apply(arg: A): Stream[B] =
    (clauses toStream).iterator map {
      _(arg)
    } takeTo {
      case CutSolution(_) => true
      case _ => false
    } flatMap (_ solutionsStream) toStream
}

