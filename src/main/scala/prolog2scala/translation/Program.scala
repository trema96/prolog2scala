package prolog2scala.translation

import treehugger.forest._
import definitions._
import prolog2scala.translation.Term.{ListTerm, Struct, Variable}
import treehuggerDSL._
import prolog2scala.translation.TranslationResult._
import treehugger.forest
import TreeHuggerUtils._
import Utils._

import scala.collection.immutable
import scala.collection.immutable.SortedSet

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[(String, Int), Seq[Clause]]) {
  private var predicateTypes: Map[StructId, (Seq[forest.Type], Seq[forest.TypeDef])] = _ //TODO molto brutto

  def translate(): TranslationResult[Tree] = {
    typing.Typing.typesOf(this) flatMap (typeInfo => {
      predicateTypes = typeInfo._1
      translationDirectives.translateManyWithContext(PredicateTranslationContext(Map.empty))((directive, ctx) =>
        translatePredicate(directive.predicateName, directive.predicateArguments map (_.direction), ctx) map (newCtx => (null, newCtx))
      ) map {case (_, PredicateTranslationContext(knownPredicates)) =>
        OBJECTDEF("TranslatedProgram") := BLOCK(typeInfo._2 ++ (knownPredicates.values map (_._2)))
      }
    })
  }

  //(predicateName, predicateArguments) => (predicateDefName, predicateDefTree)
  private type TranslatedPredicateMap = Map[(String, Seq[PredicateArgument.Direction]), (String, DefDef)]

  private def translatePredicate(
                                  predicateName: String,
                                  predicateArguments: Seq[PredicateArgument.Direction],
                                  ctx: PredicateTranslationContext
                                ): TranslationResult[PredicateTranslationContext] = {
    if (ctx.knownPredicates.contains((predicateName, predicateArguments))) {
      TranslationResult.Success(ctx)
    } else if (!predicates.contains((predicateName, predicateArguments.length))) {
      TranslationResult.Failure("There is no predicate " + predicateName + "/" + predicateArguments.length)
    } else {
      val thisPredicateTypes = predicateTypes(StructId(predicateName, predicateArguments.length))
      val defDirectiveData: Option[(String, Seq[String])] = translationDirectives filter { directive =>
        directive.predicateName == predicateName && (directive.predicateArguments map (_.direction)) == predicateArguments
      } map { directive =>
        (directive.scalaName, directive.predicateArguments filter (_.direction == PredicateArgument.Direction.In) map (_.name))
      } headOption

      val defName: String =
        defDirectiveData map (_._1) getOrElse {
          predicateName + "_" + predicateArguments.map {
            case PredicateArgument.Direction.In => "i"
            case PredicateArgument.Direction.Out => "o"
          }.mkString
        }

      val defParamNames: Seq[String] =
        defDirectiveData map (_._2) getOrElse {
          (predicateArguments filter (_ == PredicateArgument.Direction.In) zipWithIndex) map {case (_, i) => "arg" + i}
        }
      val defParamTypes: Seq[Type] = predicateArguments.zipWithIndex filter (_._1 == PredicateArgument.Direction.In) map (arg => thisPredicateTypes._1(arg._2))
      val defParams: Seq[ValDef] = defParamNames zip defParamTypes map {case (name, tp) => PARAM(name, tp)} map paramToValDef

      val defReturnType: Type = typesToSingleType(predicateArguments.zipWithIndex filter (_._1 == PredicateArgument.Direction.Out) map (arg => thisPredicateTypes._1(arg._2)))


      var defSignature =
        DEF(defName, TYPE_REF("Stream") TYPE_OF defReturnType) withParams defParams withTypeParams thisPredicateTypes._2
      if (defDirectiveData isEmpty) defSignature = defSignature withFlags Flags.PRIVATE

      predicates((predicateName, predicateArguments.length)).translateManyWithContext(
        ctx.knownPredicates + ((predicateName, predicateArguments) -> (defName, null))
      )(
        (clause, newPredicates) => translateClause(clause, predicateArguments, newPredicates)
      ) map {case (translatedClauses, newPredicates) =>
        PredicateTranslationContext(newPredicates + ((predicateName, predicateArguments) -> (defName,
          defSignature := (TYPE_REF("Predicate") TYPE_OF (typesToSingleType(defParamTypes), defReturnType) APPLY translatedClauses APPLY (defParamNames map (REF(_))))
        )))
      }
    }
  }

  private def typesToSingleType(types: Iterable[Type]): Type =
    if (types isEmpty)
      UnitClass
    else if (types.size == 1)
      types.head
    else
      TYPE_TUPLE(types)

  private def translateClauseTerm(clauseBodyTerm: Term, ctx: ClauseTermTranslationContext): TranslationResult[(ClauseTermTranslation, ClauseTermTranslationContext)] = clauseBodyTerm match {
    case Struct(name, args) =>
      //TODO operators
      val argTypes = args map { arg =>
        if (arg.variables forall ctx.knownVariables.contains)
          PredicateArgument.Direction.In
        else
          PredicateArgument.Direction.Out
      }

      translatePredicate(name, argTypes, PredicateTranslationContext(ctx.knownPredicates)) flatMap {predCtxResult =>
        val predName = predCtxResult.knownPredicates((name, argTypes))._1
        args zip argTypes filter (_._2 == PredicateArgument.Direction.In) map (_._1) translateMany argToScala flatMap { translatedArgs =>
          args zip argTypes filter (_._2 == PredicateArgument.Direction.Out) map (_._1) translateMany (argToScalaPattern(_, ctx.knownVariables)) map { translatedOuts =>
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
                               argTypes: Seq[PredicateArgument.Direction],
                               translatedPredicates: TranslatedPredicateMap
                             ): TranslationResult[(Tree, TranslatedPredicateMap)] = {
    clause.body.translateManyWithContext(
      ClauseTermTranslationContext(
        translatedPredicates,
        clause.head.args.zipWithIndex filter {case (_,i) =>
          argTypes(i) == PredicateArgument.Direction.In
        } flatMap {case (arg, _) =>
          arg.variables
        } toSet
      )
    )(translateClauseTerm) flatMap {case (translatedTerms, ClauseTermTranslationContext(knownPredicates, knownVariables)) =>
      if (clause.head.variables.forall(knownVariables.contains)) {
        clause.head.args zip argTypes filter (_._2 == PredicateArgument.Direction.In) map (_._1) translateMany (argToScalaPattern(_, Set.empty)) flatMap { translatedInputs =>
          clause.head.args zip argTypes filter (_._2 == PredicateArgument.Direction.Out) map (_._1) translateMany argToScala map { translatedOutputs =>
            val inputCase = CASE(if (translatedInputs isEmpty) WILDCARD else TUPLE(translatedInputs, flattenUnary = true))
            val outputResult = TUPLE(translatedOutputs, flattenUnary = true)

            (
              if (translatedTerms isEmpty) {
                REF("Fact") APPLY BLOCK (inputCase ==> outputResult)
              } else {
                REF("Rule") APPLY BLOCK (
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
    case ListTerm(values, None) => values.translateMany(argToScala) map (TYPE_REF("List") APPLY _)
    case ListTerm(values, Some(tail)) => argToScala(tail) flatMap (translatedTail =>
        values.translateMany(argToScala) map (REF("List") APPLY _ INFIX "++" APPLY translatedTail)
      )
    case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
  }
  private def argToScalaPattern(term: Term, knownVariables: Set[Variable]): TranslationResult[Tree] = term match {
    case Struct(name, Nil) => TranslationResult.Success(REF(structNameToScala(name)))
    case Struct(name, args) => args.translateMany(argToScalaPattern(_, knownVariables)) map (REF(structNameToScala(name)) APPLY _)
    case Variable(name) => TranslationResult.Success(if (knownVariables contains Variable(name)) BACKQUOTED(varNameToScala(name)) else ID(varNameToScala(name)))
    case ListTerm(Nil, None) => TranslationResult.Success(REF("Nil"))
    case ListTerm(values, None) => values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _))
    case ListTerm(values, Some(tail)) => argToScalaPattern(tail, knownVariables) flatMap (translatedTail =>
        values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _) UNLIST_:: translatedTail)
      )
    case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
  }

  private sealed trait ClauseTermTranslation
  private object ClauseTermTranslation {
    case class ForElement(node: Enumerator) extends ClauseTermTranslation
    case object Cut extends ClauseTermTranslation
    case class Condition(node: IfStart) extends ClauseTermTranslation
  }

  private case class ClauseTermTranslationContext(knownPredicates: TranslatedPredicateMap, knownVariables: Set[Variable])
  private case class PredicateTranslationContext(knownPredicates: TranslatedPredicateMap)
}