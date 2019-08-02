package prolog2scala.lib

sealed trait Clause[A,B] {
  def apply(arg: A): LazyList[B]
}

object Clause {
  //TODO nome migliore per partial function
  case class Rule[A, B](partialFunction: PartialFunction[A, LazyList[B]]) extends Clause[A,B] {
    override def apply(arg: A): LazyList[B] = partialFunction.applyOrElse(arg, (a: A) => LazyList())
  }

  case class Fact[A, B](partialFunction: PartialFunction[A, B]) extends Clause[A,B] {
    override def apply(arg: A): LazyList[B] = partialFunction.andThen(LazyList(_)).applyOrElse(arg, (a: A) => LazyList())
  }
}
