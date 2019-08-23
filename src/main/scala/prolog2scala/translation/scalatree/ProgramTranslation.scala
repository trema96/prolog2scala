package prolog2scala.translation.scalatree

import prolog2scala.translation.scalatree.TranslationData._
import prolog2scala.translation._
import prolog2scala.translation.TranslationResult._
import prolog2scala.translation.Term._
import prolog2scala.translation.TreeHuggerUtils._
import treehugger.forest
import forest._
import prolog2scala.translation.scalatree.TypeDefinitions._
import treehuggerDSL._

object ProgramTranslation {
  def treeOf(program: Program, predicateTypes: Map[StructId, (Seq[Type], Seq[TypeDef])]): TranslationResult[Iterable[DefDef]] = {
    def translatePredicate(predicate: PredicateTranslationId, translationData: PredicateTranslationData): TranslationResult[PredicateTranslationData] = {
      if (translationData.knownPredicates.contains(predicate)) {
        TranslationResult.Success(translationData)
      } else if (! program.predicates.contains(StructId(predicate.name, predicate.arguments.length))) {
        TranslationResult.Failure("There is no predicate " + predicate.name + "/" + predicate.arguments.length)
      } else {
        val signatureData = getPredicateSignature(predicate)

        program.predicates(StructId(predicate.name, predicate.arguments.length)).translateManyWithContext(
          translationData addPredicate (predicate -> TranslatedPredicateData(signatureData.name, null))
        )(
          (clause, newPredicates) => translateClause(clause, predicate.arguments, newPredicates)
        ) map {case (translatedClauses, newPredicates) =>
          newPredicates addPredicate  (predicate -> TranslatedPredicateData(
            signatureData.name,
            signatureData.tree := (PREDICATE(flattenedTypeTuple(signatureData.typeParams), signatureData.returnType) APPLY translatedClauses APPLY (signatureData.paramNames map (REF(_))))
          ))
        }
      }
    }

    def getPredicateSignature(predicate: PredicateTranslationId): PredicateSignatureData = {
      val thisPredicateTypes = predicateTypes(StructId(predicate.name, predicate.arguments.length))
      val directiveData: Option[(String, Seq[String])] =
        program.translationDirectives  collectFirst {
          case TranslationDirective(scalaName, predicate.name, predicateArguments) if predicateArguments.map(_.direction) == predicate.arguments =>
            (scalaName, predicateArguments filter (_.direction == PredicateArgument.Direction.In) map (_.name))
        }

      val name: String =
        directiveData map (_._1) getOrElse {
          predicate.name + "_" + predicate.arguments.map {
            case PredicateArgument.Direction.In => "i"
            case PredicateArgument.Direction.Out => "o"
          }.mkString
        }
      val paramNames: Seq[String] =
        directiveData map (_._2) getOrElse {
          (predicate.arguments filter (_ == PredicateArgument.Direction.In) zipWithIndex) map {case (_, i) => "arg" + i}
        }
      val paramTypes: Seq[Type] = predicate.arguments filterInputArgumentsIn thisPredicateTypes._1
      val params: Seq[ValDef] = paramNames zip paramTypes map {case (paramName, tp) => PARAM(paramName, tp)} map paramToValDef

      val returnType: Type = flattenedTypeTuple(predicate.arguments filterOutputArgumentsIn thisPredicateTypes._1)

      val defSignature = DEF(name, TYPE_STREAM(returnType)) withParams params withTypeParams thisPredicateTypes._2

      PredicateSignatureData(
        name,
        paramTypes,
        paramNames,
        returnType,
        if (directiveData isEmpty)
          defSignature withFlags Flags.PRIVATE
        else
          defSignature
      )
    }

    def translateClause(
                         clause: Clause,
                         argTypes: Seq[PredicateArgument.Direction],
                         translatedPredicates: PredicateTranslationData
                       ): TranslationResult[(Tree, PredicateTranslationData)] = {
      clause.body.translateManyWithContext(
        ClauseTermTranslationData(
          translatedPredicates.knownPredicates,
          argTypes filterInputArgumentsIn clause.head.args flatMap (_.variables) toSet
        )
      )(translateClauseTerm) flatMap {case (translatedTerms, ClauseTermTranslationData(knownPredicates, knownVariables)) =>
        if (clause.head.variables.forall(knownVariables.contains)) {
          argTypes filterInputArgumentsIn clause.head.args translateMany (argToScalaPattern(_, Set.empty)) flatMap { translatedInputs =>
            argTypes filterOutputArgumentsIn clause.head.args translateMany argToScala map { translatedOutputs =>
              val inputCase = CASE(if (translatedInputs isEmpty) WILDCARD else TUPLE(translatedInputs, flattenUnary = true))
              val outputResult = TUPLE(translatedOutputs, flattenUnary = true)

              (
                if (translatedTerms isEmpty) {
                  FACT (inputCase ==> outputResult)
                } else {
                  RULE (
                    inputCase ==> (FOR(
                      translatedTerms collect {
                        case ClauseTermTranslation.ForElement(node) => node
                        case ClauseTermTranslation.Cut => ???
                        case ClauseTermTranslation.Condition(_) => ???
                      }
                    ) YIELD outputResult)
                  )
                },
                PredicateTranslationData(knownPredicates)
              )
            }
          }
        } else {
          TranslationResult.Failure("Can't translate clause " + clause + " with arg types " + argTypes)
        }
      }
    }

    def translateClauseTerm(
                             clauseBodyTerm: Term,
                             translationData: ClauseTermTranslationData
                           ): TranslationResult[(ClauseTermTranslation, ClauseTermTranslationData)] = clauseBodyTerm match {
      case Struct(name, args) =>
        //TODO operators
        val argTypes = args map { arg =>
          if (arg.variables forall translationData.knownVariables.contains)
            PredicateArgument.Direction.In
          else
            PredicateArgument.Direction.Out
        }

        translatePredicate(PredicateTranslationId(name, argTypes), PredicateTranslationData(translationData.knownPredicates)) flatMap {updatedPredicateData =>
          val predName = updatedPredicateData.knownPredicates(PredicateTranslationId(name, argTypes)).scalaName
          argTypes filterInputArgumentsIn args translateMany argToScala flatMap { translatedArgs =>
            argTypes filterOutputArgumentsIn args translateMany (argToScalaPattern(_, translationData.knownVariables)) map { translatedOuts =>
              (
                ClauseTermTranslation.ForElement(VALFROM(TUPLE(translatedOuts, flattenUnary = true)) <-- (REF(predName) APPLY translatedArgs)),
                ClauseTermTranslationData(updatedPredicateData.knownPredicates, translationData.knownVariables ++ (args flatMap (_.variables)))
              )
            }
          }
        }
      case Term.Cut => ???
      case _ => TranslationResult.Failure("Term " + clauseBodyTerm + " can't be used as top level term in a clause body")
    }

    def argToScala(term: Term): TranslationResult[Tree] = term match {
      case Struct(name, Nil) => TranslationResult.Success(REF(Utils.structNameToScala(name)))
      case Struct(name, args) => args.translateMany(argToScala) map (REF(Utils.structNameToScala(name)) APPLY _)
      case Variable(name) => TranslationResult.Success(REF(Utils.varNameToScala(name)))
      case ListTerm(values, None) => values.translateMany(argToScala) map (LIST(_))
      case ListTerm(values, Some(tail)) => argToScala(tail) flatMap (translatedTail =>
        values.translateMany(argToScala) map (LIST(_) INFIX "++" APPLY translatedTail)
        )
      case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
    }

    def argToScalaPattern(term: Term, knownVariables: Set[Variable]): TranslationResult[Tree] = term match {
      case Struct(name, Nil) => TranslationResult.Success(REF(Utils.structNameToScala(name)))
      case Struct(name, args) => args.translateMany(argToScalaPattern(_, knownVariables)) map (REF(Utils.structNameToScala(name)) APPLY _)
      case Variable(name) => TranslationResult.Success(if (knownVariables contains Variable(name)) BACKQUOTED(Utils.varNameToScala(name)) else ID(Utils.varNameToScala(name)))
      case ListTerm(Nil, None) => TranslationResult.Success(NIL)
      case ListTerm(values, None) => values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _))
      case ListTerm(values, Some(tail)) => argToScalaPattern(tail, knownVariables) flatMap (translatedTail =>
          values.translateMany(argToScalaPattern(_, knownVariables)) map (INFIX_CHAIN("::", _) UNLIST_:: translatedTail)
        )
      case _ => TranslationResult.Failure("Can't use term " + term + " as an argument")
    }

    program.translationDirectives.translateFold(PredicateTranslationData(Map.empty)){(directive, updatedData) =>
      translatePredicate(PredicateTranslationId(directive.predicateName, directive.predicateArguments map (_.direction)), updatedData)
    } map { predicatesTranslationData =>
      predicatesTranslationData.knownPredicates.values map (_.predicateDefinition)
    }
  }

  implicit class RichArgumentTypeSeq(base: Seq[PredicateArgument.Direction]) {
    def filterInputArgumentsIn[A](argData: Seq[A]): Seq[A] = filterArguments(argData, PredicateArgument.Direction.In)
    def filterOutputArgumentsIn[A](argData: Seq[A]): Seq[A] = filterArguments(argData, PredicateArgument.Direction.Out)
    private def filterArguments[A](argData: Seq[A], desiredType: PredicateArgument.Direction): Seq[A] = {
      require(base.length == argData.length, "The argument data sequence should have the same number of elements as the argument types sequence")

      base zip argData filter (_._1 == desiredType) map (_._2)
    }
  }
}
