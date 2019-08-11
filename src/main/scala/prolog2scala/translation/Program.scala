package prolog2scala.translation

import treehugger.forest._
import definitions._
import prolog2scala.translation.Term.{ListTerm, Struct, Variable}
import treehuggerDSL._
import prolog2scala.translation.TranslationResult._

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[(String, Int), Seq[Clause]]) {
  def translate(program: Program): Unit = ???

  private type TranslatedPredicateMap = Map[(String, Seq[PredicateArgument.Type]), (String, Tree)]

  private def translatePredicate(predicateName: String, predicateArguments: Seq[PredicateArgument.Type], ctx: PredicateTranslationContext): TranslationResult[PredicateTranslationContext] = ???

  val x = FOR(VALFROM(TUPLE(REF("x"))) := LIT(0) INT_TO LIT(2))

  private def translateClauseTerm(clauseBodyTerm: Term, ctx: ClauseTermTranslationContext): TranslationResult[(ClauseTermTranslation, ClauseTermTranslationContext)] = clauseBodyTerm match {
    case Struct(name, args) =>
      //TODO operators
      val argTypes = args map { arg =>
        if (arg.variables forall ctx.knownVariables.contains)
          PredicateArgument.Type.In
        else
          PredicateArgument.Type.Out
      }

      translatePredicate(name, argTypes, PredicateTranslationContext(ctx.knownPredicates)) flatMap {predCtxResult =>
        val predName = predCtxResult.knownPredicates((name, argTypes))._1
        args zip argTypes filter (_._2 == PredicateArgument.Type.In) map (_._1) translateMany argToScala flatMap {translatedArgs =>
          args zip argTypes filter (_._2 == PredicateArgument.Type.Out) map (_._1) translateMany (argToScalaPattern(_, ctx.knownVariables)) map {translatedOuts =>
            (ClauseTermTranslation.ForElement(TUPLE(translatedOuts, flattenUnary = true) := REF(predName) APPLY translatedArgs),
              ClauseTermTranslationContext(predCtxResult.knownPredicates, ctx.knownVariables ++ (args flatMap (_.variables))))
          }
        }
      }
    case _ => TranslationResult.Failure("Term " + clauseBodyTerm + " can't be used as top level term in a clause body")
  }

  private def translateClause(
                       clause: Clause,
                       argTypes: Seq[PredicateArgument.Type],
                       translatedPredicates: TranslatedPredicateMap
                     ) = {
    clause.body.translateManyWithContext(
      ClauseTermTranslationContext(
        translatedPredicates,
        clause.head.args.zipWithIndex filter {case (_,i) =>
          argTypes(i) == PredicateArgument.Type.In
        } flatMap {case (arg, _) =>
          arg.variables
        } toSet
      )
    )(translateClauseTerm) flatMap {case (translatedTerms, ClauseTermTranslationContext(knownPredicates, knownVariables)) =>
      if (clause.head.variables.forall(knownVariables.contains)) {
        clause.head.args translateMany argToScala map {translatedHeadArgs =>
          val inputs = translatedHeadArgs.zip(argTypes) filter (_._2 == PredicateArgument.Type.Out) map (_._1)
          val outputs = translatedHeadArgs.zip(argTypes) filter (_._2 == PredicateArgument.Type.Out) map (_._1)
          val inputCase = CASE(if (inputs isEmpty) WILDCARD else TUPLE(inputs, flattenUnary = true))
          val outputResult = TUPLE(outputs, flattenUnary = true)

          if (translatedTerms isEmpty) {
            REF("Fact") APPLY BLOCK(inputCase ==> outputResult)
          } else {
            FOR(
              translatedTerms collect {
                case ClauseTermTranslation.ForElement(node) => node
                case ClauseTermTranslation.Cut => ???
                case ClauseTermTranslation.Condition(_) => ???
              }
            ) YIELD outputResult
          }
        }
      } else {
        TranslationResult.Failure("Can't translate clause " + clause + " with arg types " + argTypes)
      }
    }
  }

  private def argToScala(term: Term): TranslationResult[Tree] = term match {
    //TODO what if name is not a valid scala name? Keep track of chosen names with map?
    case Struct(name, Nil) => TranslationResult.Success(REF(structNameToScala(name)))
    case Struct(name, args) => args.translateMany(argToScala) map (REF(structNameToScala(name)) APPLY _)
    case Variable(name) => TranslationResult.Success(REF(varNameToScala(name)))
    case ListTerm(values, None) => values.translateMany(argToScala) map (TYPE_LIST(AnyClass) APPLY _)
    case ListTerm(values, Some(tail)) => argToScala(tail) flatMap (translatedTail =>
        values.translateMany(argToScala) map (TYPE_LIST(AnyClass) APPLY _ INFIX "++" APPLY translatedTail)
      )
    case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
  }
  private def argToScalaPattern(term: Term, knownVariables: Set[Variable]): TranslationResult[Tree] = term match {
    case Struct(name, Nil) => TranslationResult.Success(REF(structNameToScala(name)))
    case Struct(name, args) => args.translateMany(argToScalaPattern(_, knownVariables)) map (REF(structNameToScala(name)) APPLY _)
    case Variable(name) => TranslationResult.Success(if (knownVariables contains Variable(name)) BACKQUOTED(varNameToScala(name)) else ID(varNameToScala(name)))
    case ListTerm(values, None) => values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _))
    case ListTerm(values, Some(tail)) => argToScalaPattern(tail, knownVariables) flatMap (translatedTail =>
        values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _) UNLIST_:: translatedTail)
      )
    case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
  }

  private def structNameToScala(name: String): String = name.substring(0,1).toLowerCase + name.substring(1)
  private def varNameToScala(name: String): String = name.substring(0,1).toUpperCase + name.substring(1)


  private sealed trait ClauseTermTranslation
  private object ClauseTermTranslation {
    case class ForElement(node: Enumerator) extends ClauseTermTranslation
    case object Cut extends ClauseTermTranslation
    case class Condition(node: IfStart) extends ClauseTermTranslation
  }

  private case class ClauseTermTranslationContext(knownPredicates: TranslatedPredicateMap, knownVariables: Set[Variable])
  private case class PredicateTranslationContext(knownPredicates: TranslatedPredicateMap)
  /*
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
        case OldTranslationResult.Failure(msg) => OldTranslationResult.Failure(msg)
      })
    } getOrElse OldTranslationResult.Failure("There is no predicate " + predName + "/" + predArgsTypes.length)*/
}