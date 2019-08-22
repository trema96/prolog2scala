package prolog2scala.translation.scalatree

import prolog2scala.translation.PredicateArgument
import prolog2scala.translation.Term.Variable
import treehugger.forest._
import treehuggerDSL._

object TranslationData {
  type PredicateTranslationMap = Map[PredicateTranslationId, TranslatedPredicateData]

  case class PredicateTranslationId(name: String, arguments: Seq[PredicateArgument.Direction])
  case class TranslatedPredicateData(scalaName: String, predicateDefinition: DefDef)

  case class ClauseTermTranslationData(knownPredicates: PredicateTranslationMap, knownVariables: Set[Variable]) {
    def addKnownVariables(toAdd: Iterable[Variable]): ClauseTermTranslationData = ClauseTermTranslationData(knownPredicates, knownVariables ++ toAdd)
  }

  case class PredicateTranslationData(knownPredicates: PredicateTranslationMap) {
    def addPredicate(newEntry: (PredicateTranslationId, TranslatedPredicateData)): PredicateTranslationData = PredicateTranslationData(knownPredicates + newEntry)
  }

  case class PredicateSignatureData(name: String, typeParams: Seq[Type], paramNames: Seq[String], returnType: Type, tree: DefTreeStart)
}
