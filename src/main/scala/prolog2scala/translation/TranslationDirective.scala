package prolog2scala.translation

case class TranslationDirective(scalaName: String, predicateName: String, predicateArguments: Seq[PredicateArgument])

case class PredicateArgument(name: String, predType: PredicateArgument.Type)

object PredicateArgument {
  sealed trait Type
  object Type {
    case object In extends Type
    case object Out extends Type
  }
}

