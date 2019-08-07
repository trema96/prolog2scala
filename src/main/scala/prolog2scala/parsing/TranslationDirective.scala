package prolog2scala.parsing

case class TranslationDirective(scalaName: String, predicateName: String, predicateArguments: Seq[PredicateArgument])

sealed trait PredicateArgument

object PredicateArgument {
  case class In(name: String) extends PredicateArgument
  case class Out(name: String) extends PredicateArgument
}
