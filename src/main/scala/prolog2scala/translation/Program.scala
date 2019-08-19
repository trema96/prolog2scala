package prolog2scala.translation

import treehugger.forest._
import definitions._
import prolog2scala.translation.ClauseTermTypeCheckContext
import prolog2scala.translation.TypeCheck._
import prolog2scala.translation.ArgumentType._
import prolog2scala.translation.Term.{ListTerm, Struct, Variable}
import treehuggerDSL._
import prolog2scala.translation.TranslationResult._
import treehugger.forest

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[(String, Int), Seq[Clause]]) {
  def translate(): TranslationResult[Tree] =
    translationDirectives.translateManyWithContext(PredicateTranslationContext(Map.empty))((directive, ctx) =>
      translatePredicate(directive.predicateName, directive.predicateArguments map (_.direction), ctx) map (newCtx => (null, newCtx))
    ) map {case (_, PredicateTranslationContext(knownPredicates)) =>
      OBJECTDEF("TranslatedProgram") := BLOCK(knownPredicates.values map (_._2))
    }

  //(predicateName, predicateArguments) => (predicateDefName, predicateDefTree)
  private type TranslatedPredicateMap = Map[(String, Seq[PredicateArgument.Direction]), (String, DefDef)]

  private def translatePredicate(predicateName: String, predicateArguments: Seq[PredicateArgument.Direction], ctx: PredicateTranslationContext): TranslationResult[PredicateTranslationContext] = {
    implicit def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
    implicit def paramSeqToValDef(param: Seq[ValNameStart]): Seq[ValDef] = param map paramToValDef

    if (ctx.knownPredicates.contains((predicateName, predicateArguments))) {
      TranslationResult.Success(ctx)
    } else if (!predicates.contains((predicateName, predicateArguments.length))) {
      TranslationResult.Failure("There is no predicate " + predicateName + "/" + predicateArguments.length)
    } else {
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
      val defParamTypes: Seq[Type] = defParamNames map (_ => AnyClass)
      val defParams: Seq[ValDef] = defParamNames zip defParamTypes map {case (name, tp) => PARAM(name, tp)}

      val defReturnTypes: Type = TYPE_TUPLE(predicateArguments filter {_ == PredicateArgument.Direction.Out} map { _ => AnyClass})

      var defSignature = DEF(defName, TYPE_REF("Stream") TYPE_OF defReturnTypes) withParams defParams
      if (defDirectiveData isEmpty) defSignature = defSignature withFlags Flags.PRIVATE

      predicates((predicateName, predicateArguments.length)).translateManyWithContext(
        ctx.knownPredicates + ((predicateName, predicateArguments) -> (defName, null))
      )(
        (clause, newPredicates) => translateClause(clause, predicateArguments, newPredicates)
      ) map {case (translatedClauses, newPredicates) =>
        PredicateTranslationContext(newPredicates + ((predicateName, predicateArguments) -> (defName,
          defSignature := (TYPE_REF("Predicate") TYPE_OF (TYPE_TUPLE(defParamTypes), defReturnTypes) APPLY translatedClauses APPLY (defParamNames map (REF(_))))
        )))
      }
    }
  }

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
    case ListTerm(values, None) => values.translateMany(argToScala) map (TYPE_LIST(AnyClass) APPLY _)
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

  //TYPECHECK
  /*
  var program =
    """
      |#lookup: lookup(+list, -elem, -position, -listNoElem)
      |lookup([H|T],H,zero,T).
      |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
    """.stripMargin
  program =
    """
      |#permutation: permutation(+list, -permutations)
      |member2([X|Xs],X,Xs).
      |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
      |permutation([],[]).
      |permutation(Xs,[X|Ys]) :-
      | member2(Xs,X,Zs),
      | permutation(Zs, Ys).
    """.stripMargin
    */

  def typeCheck() = {
    (predicates.values.flatten toSeq) translateMany typeCheckClause map (_ joinMany) map { typeData =>
      val cleanPredicates = typeData.predicates.foldLeft(typeData.predicates)((predMap, currEntry) =>
        currEntry._2.zipWithIndex.foldLeft(predMap)((argPredMap, currArg) => {
          val currArgReplaced = currArg._1.replaceArguments(argPredMap, currEntry._1._1, currEntry._1._2, currArg._2)
          currArgReplaced._2 + (currEntry._1 -> currArgReplaced._2(currEntry._1).updated(currArg._2, currArgReplaced._1))
        })
      )
      val typesEquivalences = EquivalenceGroups.empty joinMany cleanPredicates.values.flatMap(_.map(_.freeTypeEquivalences))
      println(typeData.predicates)
      println(cleanPredicates)
      println(typesEquivalences)
      var traitMap: TraitMap = Map.empty
      val predTypeMap = cleanPredicates.map{ case (key, value) =>
        (
          key,
          value.zipWithIndex.foldLeft[(Seq[Type], FreeTypeMap)]((Seq.empty, Map.empty))((ctx, currTypes) => {
            val ancestorData = currTypes._1.leastCommonAncestor(
              ctx._2,
              typesEquivalences,
              typeData.structs,
              traitMap,
              translationDirectives collectFirst {
                case TranslationDirective(scalaName, key._1, args) if args.length == key._2 => scalaName + "_" + args(currTypes._2).name
              } getOrElse (key._1 + "_" + key._2 + "_arg" + currTypes._2)
            )
            traitMap = ancestorData._3
            (ctx._1 :+ ancestorData._1, ancestorData._2)
          })._1
        )
      }
      //val cleanStructs = typeData.structs.map{case (key, value) => (key, value.zipWithIndex.map(arg => arg._1.replaceArguments(cleanPredicates)))}

      predTypeMap
    }
  }

  private def typeCheckClause(clause: Clause): TranslationResult[TypeCheckContext] = {
    (clause.head +: clause.body.collect{case x: Struct => x}) translateMany typeCheckTopLevelStruct map (_ joinMany) map { ctx =>
      var varMap = ctx.vars
      var nextToReplace = varMap.find(_._2 exists (_.hasVariable))
      while (nextToReplace isDefined) {
        val replaceResult = nextToReplace.get._2.replaceVariables(varMap, nextToReplace.get._1)
        varMap = replaceResult._2 + (nextToReplace.get._1 -> replaceResult._1)
        nextToReplace = varMap.find(_._2 exists (_.hasVariable))
      }
      TypeCheckContext(
        ctx.predicates map { case (key, value) =>
          (key, value map (_.replaceVariables(varMap)))
        },
       ctx.structs map { case (key, value) =>
        (key, value map (_.replaceVariables(varMap)))
        }
      )
    }
  }

  private def typeCheckTopLevelStruct(struct: Struct): TranslationResult[ClauseTypeCheckContext] =
    struct.args.zipWithIndex.translateManyWithContext(ClauseTermTypeCheckContext.empty)((arg, ctx) => arg._1 match {
      case Variable(name) => TranslationResult.Success((TypeOfVar(name), ctx joinVar Map(name -> Set[ArgumentType](TypeOfArg(struct.name, struct.args.length, arg._2)))))
      case _ => typeCheckTerm(arg._1) map {case (argType, argCtx) => (argType, ctx join argCtx)}
    }) map { case (headArgsTypes, headCtx) =>
      ClauseTypeCheckContext(Map((struct.name, struct.args.length) -> (headArgsTypes map (Set(_)))), headCtx.structs, headCtx.vars)
    }

  private def typeCheckTerm(term: Term): TranslationResult[(ArgumentType, ClauseTermTypeCheckContext)] = term match {
    case Struct(name, args) => args translateMany typeCheckTerm map { translatedArgs =>
      (
        StructType(name, args.length),
        (translatedArgs map(_._2) joinMany) joinStruct Map((name, args.length) -> (translatedArgs map (_._1) map (Set(_))))
      )
    }
    case ListTerm(values, tail) =>
      values translateMany typeCheckTerm map { translatedValues =>
        val listType: ArgumentType = ListType(translatedValues map (_._1) toSet)
        (
          listType,
          (translatedValues map (_._2) joinMany) joinVar (tail map (v => Map(v.name -> Set(listType))) getOrElse Map.empty)
        )
      }
    case Variable(name) => TranslationResult.Success((TypeOfVar(name), ClauseTermTypeCheckContext.empty))
    case _ => TranslationResult.Failure("Invalid term: " + term)
  }
}