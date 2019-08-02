package prolog2scala.lib

case class Predicate[A,B](clauses: Clause[A,B]*) {
  def apply(arg: A): LazyList[B] = clauses to LazyList flatMap {_(arg)}
}

