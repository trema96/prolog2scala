package prolog2scala.lib

object Cut {
  case class CutResult[A](solutions: LazyList[A], didCut: Boolean) {
    def ![B](other: A => LazyList[B]): CutResult[B] = CutResult(solutions.take(1).flatMap(other), didCut)

    def ! : CutResult[A] = CutResult(solutions.take(1), didCut)
  }

  implicit class LazyListCut[A](base: LazyList[A]) {
    def ![B](other: A => LazyList[B]): CutResult[B] = {
      val cutBase = base.take(1)
      CutResult(cutBase.flatMap(other), cutBase.nonEmpty)
    }

    def unary_! : CutResult[A] = CutResult(base, didCut = true)

    def ! : CutResult[A] = CutResult(base.take(1), base.nonEmpty)
  }
}