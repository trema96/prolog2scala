package prolog2scala.translation

sealed trait TranslationResult[+A] {
  def flatMap[B](f: A => TranslationResult[B]): TranslationResult[B]
  def map[B](f: A => B): TranslationResult[B] = flatMap(x => TranslationResult.Success(f(x)))
}

object TranslationResult {
  case class Failure(msg: String) extends TranslationResult[Nothing] {
    override def flatMap[B](f: Nothing => TranslationResult[B]): TranslationResult[B] = this
  }
  case class Success[A](value: A) extends TranslationResult[A] {
    override def flatMap[B](f: A => TranslationResult[B]): TranslationResult[B] = f(value)
  }

  implicit class TranslationSeq[A](base: Seq[A]) {
    def translateMany[B](f: A => TranslationResult[B]): TranslationResult[Seq[B]] =
      translateManyWithContext(null)((e, _) => f(e).map((_, null))).map(_._1)

    def translateManyWithContext[B, C](initialCtx: C)(f: (A, C) => TranslationResult[(B, C)]): TranslationResult[(Seq[B], C)] = {
      def translateManyWithContextRec(ctx: C, elems: Seq[A]): TranslationResult[(Seq[B], C)] = elems toList match {
        case Nil => TranslationResult.Success((Nil, ctx))
        case h :: t => f(h, ctx) flatMap {case (h2, newCtx) =>
            translateManyWithContextRec(newCtx, t) map {case (t2, resCtx) => (h2 +: t2, resCtx)}
        }
      }
      translateManyWithContextRec(initialCtx, base)
    }

    def translateFold[B](initialVal: B)(f: (A, B) => TranslationResult[B]): TranslationResult[B] =
      translateManyWithContext(initialVal)((a, b) => f(a, b) map ((null, _))) map (_._2)
  }
}