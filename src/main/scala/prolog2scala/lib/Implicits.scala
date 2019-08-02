package prolog2scala.lib

object Implicits {
  implicit class LazyListExtensions[A](base: LazyList[A]) {
    //TODO generalizzabile?
    /**
      * Take elements until the provided predicate is satisfied (includes the last element)
      * @param p a predicate
      * @return a lazy list where
      */
    def takeUntil(p: A => Boolean): LazyList[A] = {
      new Iterator[A]{
        private val baseIterator: Iterator[A] = base.iterator
        private var latest: Option[A] = None

        override def hasNext: Boolean = latest.forall(!p(_)) && baseIterator.hasNext

        override def next(): A = {
          val res: A = baseIterator.next()
          latest = Some(res)
          res
        }
      }
    }.to(LazyList)
  }
}
