package prolog2scala.lib

object Implicits {
  implicit class RichIterator[A](base: Iterator[A]) {
    /**
      * Take elements until the provided predicate is satisfied (includes the last element)
      * @param p a predicate
      * @return the iterator were the last element is the first element satisfying the predicate
      */
    def takeTo(p: A => Boolean): Iterator[A] = {
      new Iterator[A]{
        private var latest: Option[A] = None

        override def hasNext: Boolean = latest.forall(!p(_)) && base.hasNext

        override def next(): A = {
          val res: A = base.next()
          latest = Some(res)
          res
        }
      }
    }
  }
}
