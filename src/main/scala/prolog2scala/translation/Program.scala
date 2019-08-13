package prolog2scala.translation

import treehugger.forest._
import definitions._
import prolog2scala.translation.Term.{ListTerm, Struct, Variable}
import treehuggerDSL._
import prolog2scala.translation.TranslationResult._

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[(String, Int), Seq[Clause]]) {
  def translate(): TranslationResult[Tree] =
    translationDirectives.translateManyWithContext(PredicateTranslationContext(Map.empty))((directive, ctx) =>
      translatePredicate(directive.predicateName, directive.predicateArguments map (_.predType), ctx) map (newCtx => (null, newCtx))
    ) map {case (_, PredicateTranslationContext(knownPredicates)) =>
      OBJECTDEF("TranslatedProgram") := BLOCK(knownPredicates.values map (_._2))
    }

  private type TranslatedPredicateMap = Map[(String, Seq[PredicateArgument.Type]), (String, DefDef)]

  private def translatePredicate(predicateName: String, predicateArguments: Seq[PredicateArgument.Type], ctx: PredicateTranslationContext): TranslationResult[PredicateTranslationContext] = {
    implicit def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
    implicit def paramSeqToValDef(param: Seq[ValNameStart]): Seq[ValDef] = param map paramToValDef

    if (ctx.knownPredicates.contains((predicateName, predicateArguments))) {
      TranslationResult.Success(ctx)
    } else if (!predicates.contains((predicateName, predicateArguments.length))) {
      TranslationResult.Failure("There is no predicate " + predicateName + "/" + predicateArguments.length)
    } else {
      val defDirectiveData: Option[(String, Seq[String])] = translationDirectives filter { directive =>
        directive.predicateName == predicateName && (directive.predicateArguments map (_.predType)) == predicateArguments
      } map { directive =>
        (directive.scalaName, directive.predicateArguments filter (_.predType == PredicateArgument.Type.In) map (_.name))
      } headOption

      val defName: String =
        defDirectiveData map (_._1) getOrElse {
          predicateName + "_" + predicateArguments.map {
            case PredicateArgument.Type.In => "i"
            case PredicateArgument.Type.Out => "o"
          }.mkString
        }

      val defParamNames: Seq[String] =
        defDirectiveData map (_._2) getOrElse {
          (predicateArguments filter (_ == PredicateArgument.Type.In) zipWithIndex) map {case (_, i) => "arg" + i}
        }
      val defParamTypes: Seq[Type] = defParamNames map (_ => AnyClass)
      val defParams: Seq[ValDef] = defParamNames zip defParamTypes map {case (name, tp) => PARAM(name, tp)}

      val defReturn: Type = TYPE_REF("Stream") TYPE_OF TYPE_TUPLE(predicateArguments filter {_ == PredicateArgument.Type.Out} map {_ => AnyClass})//TODO non in predicate

      var defSignature = DEF(defName, defReturn) withParams defParams
      if (defDirectiveData isEmpty) defSignature = defSignature withFlags Flags.PRIVATE

      predicates((predicateName, predicateArguments.length)).translateManyWithContext(
        ctx.knownPredicates + ((predicateName, predicateArguments) -> (defName, null))
      )(
        (clause, newPredicates) => translateClause(clause, predicateArguments, newPredicates)
      ) map {case (translatedClauses, newPredicates) =>
        PredicateTranslationContext(newPredicates + ((predicateName, predicateArguments) -> (defName,
          defSignature := (TYPE_REF("Predicate") TYPE_OF (TYPE_TUPLE(defParamTypes), defReturn) APPLY translatedClauses APPLY (defParamNames map (REF(_))))
        )))
      }
    }
  }

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
            (
              ClauseTermTranslation.ForElement(VALFROM(TUPLE(translatedOuts, flattenUnary = true)) <-- (REF(predName) APPLY translatedArgs)),
              ClauseTermTranslationContext(predCtxResult.knownPredicates, ctx.knownVariables ++ (args flatMap (_.variables)))
            )
          }
        }
      }
    case Term.Cut => ???
    case _ => TranslationResult.Failure("Term " + clauseBodyTerm + " can't be used as top level term in a clause body")
  }

  private def translateClause(
                       clause: Clause,
                       argTypes: Seq[PredicateArgument.Type],
                       translatedPredicates: TranslatedPredicateMap
                             ): TranslationResult[(Tree, TranslatedPredicateMap)] = {
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
        clause.head.args zip argTypes filter (_._2 == PredicateArgument.Type.In) map (_._1) translateMany (argToScalaPattern(_, Set.empty)) flatMap { translatedInputs =>
          clause.head.args zip argTypes filter (_._2 == PredicateArgument.Type.Out) map (_._1) translateMany argToScala map { translatedOutputs =>
            val inputCase = CASE(if (translatedInputs isEmpty) WILDCARD else TUPLE(translatedInputs, flattenUnary = true))
            val outputResult = TUPLE(translatedOutputs, flattenUnary = true)

            (
              if (translatedTerms isEmpty) {
                REF("Fact") BLOCK(inputCase ==> outputResult)
              } else {
                REF("Rule") BLOCK(
                  inputCase ==> (FOR(
                    translatedTerms collect {
                      case ClauseTermTranslation.ForElement(node) => node
                      case ClauseTermTranslation.Cut => ???
                      case ClauseTermTranslation.Condition(_) => ???
                    }
                  ) YIELD outputResult)
                )
              },
              knownPredicates
            )
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

  private def structNameToScala(name: String): String = name.substring(0,1).toUpperCase + name.substring(1)
  private def varNameToScala(name: String): String = name.substring(0,1).toLowerCase + name.substring(1)


  private sealed trait ClauseTermTranslation
  private object ClauseTermTranslation {
    case class ForElement(node: Enumerator) extends ClauseTermTranslation
    case object Cut extends ClauseTermTranslation
    case class Condition(node: IfStart) extends ClauseTermTranslation
  }

  private case class ClauseTermTranslationContext(knownPredicates: TranslatedPredicateMap, knownVariables: Set[Variable])
  private case class PredicateTranslationContext(knownPredicates: TranslatedPredicateMap)
}