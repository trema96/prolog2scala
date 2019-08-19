package prolog2scala.translation

case class TranslationDirective(scalaName: String, predicateName: String, predicateArguments: Seq[PredicateArgument])

case class PredicateArgument(name: String, direction: PredicateArgument.Direction)

object PredicateArgument {
  sealed trait Direction
  object Direction {
    case object In extends Direction
    case object Out extends Direction
  }
}

