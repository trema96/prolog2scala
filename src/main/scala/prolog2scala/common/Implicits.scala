package prolog2scala.common

object Implicits {
  implicit class IteratorExtended[A](base: Iterator[A]) {
    //TODO generalizzabile?
    /**
      * Take elements until the provided predicate is satisfied (includes the last element)
      * @param p a predicate
      * @return the iterator were the last element is the first element satisfying the predicate
      */
    def takeTo(p: A => Boolean): Iterator[A] = {
      new Iterator[A]{
        private val baseIterator: Iterator[A] = base
        private var latest: Option[A] = None

        override def hasNext: Boolean = latest.forall(!p(_)) && baseIterator.hasNext

        override def next(): A = {
          val res: A = baseIterator.next()
          latest = Some(res)
          res
        }
      }
    }
  }
}
