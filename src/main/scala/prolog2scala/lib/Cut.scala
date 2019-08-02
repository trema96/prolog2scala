package prolog2scala.lib

object Cut {
  case class CutSolutions[A](solutions: LazyList[A], didCut: Boolean) {
    def ![B](other: A => LazyList[B]): CutSolutions[B] = CutSolutions(solutions.take(1).flatMap(other), didCut)

    def ! : CutSolutions[A] = CutSolutions(solutions.take(1), didCut)
  }

  implicit class LazyListCut[A](base: LazyList[A]) {
    def ![B](other: A => LazyList[B]): CutSolutions[B] = {
      val cutBase = base.take(1)
      CutSolutions(cutBase.flatMap(other), cutBase.nonEmpty)
    }

    def unary_! : CutSolutions[A] = CutSolutions(base, didCut = true)

    def ! : CutSolutions[A] = CutSolutions(base.take(1), base.nonEmpty)
  }
}