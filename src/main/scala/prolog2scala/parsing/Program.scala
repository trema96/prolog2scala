package prolog2scala.parsing

import prolog2scala.parsing.Term.{Cut, Struct, Variable}

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[(String, Int), Seq[Clause]]) {
  def translate(): PredicateTranslationResult =
    translationDirectives.foldLeft[PredicateTranslationResult](PredicateTranslationResult.Success(Set.empty))((prevResult, directive) => prevResult match {
      case PredicateTranslationResult.Success(knownPredicates) =>
        translatePredicate(directive.predicateName, directive.predicateArguments map (_.predType), knownPredicates)
      case x => x
    })

  private def translatePredicate(
                                  predName: String,
                                  predArgsTypes: Seq[PredicateArgument.Type],
                                  translatedPredicates: Set[(String, Seq[PredicateArgument.Type])]
                                ): PredicateTranslationResult =
    predicates.get((predName, predArgsTypes.length)) map {predicateClauses =>
      predicateClauses.foldLeft[PredicateTranslationResult](PredicateTranslationResult.Success(
        translatedPredicates + ((predName, predArgsTypes)))
      )((prevResult, clause) => prevResult match {
        case PredicateTranslationResult.Success(knownPredicates) => TranslateClause(clause, predArgsTypes, knownPredicates)
        case TranslationResult.Failure(msg) => TranslationResult.Failure(msg)
      })
    } getOrElse TranslationResult.Failure("There is no predicate " + predName + "/" + predArgsTypes.length)

  private def TranslateClause(
                               clause: Clause,
                               predArgsTypes: Seq[PredicateArgument.Type],
                               translatedPredicates: Set[(String, Seq[PredicateArgument.Type])]
                             ): PredicateTranslationResult = {
    val clauseResult = clause.body.foldLeft[ClauseTranslationResult](
      ClauseTranslationResult.Success(
        translatedPredicates,
        clause.head.args.zipWithIndex filter {case (_,i) =>
          predArgsTypes(i) == PredicateArgument.Type.In
        } flatMap {case (arg, _) =>
          arg.variables
        } toSet
      )
    )((prevResult, clauseBodyTerm) => prevResult match {
      case ClauseTranslationResult.Success(knownPredicates, knownVariables) => clauseBodyTerm match {
        case Struct(name, args) =>
          //TODO operation
          val structArgTypes: Seq[PredicateArgument.Type] = args map (arg =>
            if (arg.variables.forall(knownVariables.contains))
              PredicateArgument.Type.In
            else
              PredicateArgument.Type.Out
            )
          val newVariables = knownVariables ++ (args flatMap (_.variables) toSet)
          if (knownPredicates contains ((name, structArgTypes))) {
            ClauseTranslationResult.Success(knownPredicates, newVariables)
          } else {
            translatePredicate(name, structArgTypes, knownPredicates) match {
              case PredicateTranslationResult.Success(newPredicates) =>
                ClauseTranslationResult.Success(newPredicates, newVariables)
              case TranslationResult.Failure(msg) => TranslationResult.Failure(msg)
            }
          }
        case Cut => prevResult
        case _ => TranslationResult.Failure("Only cut, predicates and operation allowed in a clause body")
      }
      case TranslationResult.Failure(msg) => TranslationResult.Failure(msg)
    })
    clauseResult match {
      case ClauseTranslationResult.Success(knownPredicates, knownVariables) =>
        if (clause.head.variables.forall(knownVariables.contains)) {
          PredicateTranslationResult.Success(knownPredicates)
        } else {
          TranslationResult.Failure("Can't translate clause " + clause + " with predicat arguments of type " + predArgsTypes)
        }
      case TranslationResult.Failure(msg) => TranslationResult.Failure(msg)
    }
  }
}

sealed trait TranslationResult
sealed trait PredicateTranslationResult

object PredicateTranslationResult {
  case class Success(knownPredicates: Set[(String, Seq[PredicateArgument.Type])]) extends PredicateTranslationResult
}

sealed trait ClauseTranslationResult

object ClauseTranslationResult {
  case class Success(knownPredicates: Set[(String, Seq[PredicateArgument.Type])], knownVariables: Set[Variable]) extends ClauseTranslationResult
}

object TranslationResult {
  case class Failure(msg: String) extends TranslationResult with PredicateTranslationResult with ClauseTranslationResult
}