package prolog2scala.translation.typing

import prolog2scala.translation._
import prolog2scala.translation.Term._
import prolog2scala.translation.TranslationResult._
import prolog2scala.translation.TreeHuggerUtils._
import prolog2scala.translation.typing.ArgumentType._
import prolog2scala.translation.typing.TypeData._
import prolog2scala.translation.typing.Merge._
import prolog2scala.translation.typing.DataMaps._
import treehugger.forest._
import treehuggerDSL._

object ProgramTyping {
  /**
    * Tryes to infer best types for each of this program predicates and the necessary struct types
    * @param program the program to analyze
    * @return (a map containing the types for each predicate argument and its type arguments, all known struct classes and traits tree)
    */
  def typesOf(program: Program): TranslationResult[(Map[StructId, (Seq[Type], Seq[TypeDef])], Iterable[Tree])] = {
    decideProgramTypes(program) map { case (predTypeMap, structTypeMap, traitMap) =>
      val structDefs: Iterable[Tree] = structTypeMap map { case (struct, args) =>
        if (args nonEmpty) {
          CASECLASSDEF(Utils.structToScalaName(struct, structTypeMap.keys)) withParams
            args.zipWithIndex.map(arg => PARAM("arg" + arg._2, arg._1.treeType)).map(paramToValDef) withParents
            traitMap.collect{case (key, value) if key.contains(StructType(struct.name, struct.arity)) => value.treeType} withTypeParams
            args.flatMap(_.typeArg).distinct.map(_.typeDef)
        } else {
          CASEOBJECTDEF(Utils.structToScalaName(struct, structTypeMap.keys)) withParents
            traitMap.collect{case (key, value) if key.contains(StructType(struct.name, struct.arity)) => value.treeType}
        }
      } map (_.mkTree(EmptyTree))
      val traitsDef: Iterable[ClassDef] = traitMap.values map { trt => TRAITDEF(trt.name)} map toEmptyClassDef
      (
        predTypeMap.mapValues(values => (values.map(_.treeType), values.flatMap(_.typeArg).distinct.map(_.typeDef))),
        traitsDef ++ structDefs
      )
    }
  }

  private def decideProgramTypes(program: Program): TranslationResult[(Map[StructId, Seq[DecidedArgumentType]], Map[StructId, Seq[DecidedArgumentType]], TraitMap)] = {
    (program.predicates.values.flatten toSeq) translateMany typesOfClause map (mergeMany(_)) map { typeData =>
      val cleanPredicates = typeData.predicateTypes.foldLeft(typeData.predicateTypes)((predMap, currEntry) =>
        currEntry._2.zipWithIndex.foldLeft(predMap)((argPredMap, currArg) => {
          val currArgReplaced = currArg._1.replaceArguments(argPredMap, currEntry._1.name, currEntry._1.arity, currArg._2)
          currArgReplaced._2 + (currEntry._1 -> currArgReplaced._2(currEntry._1).updated(currArg._2, currArgReplaced._1))
        })
      )
      val typesEquivalences = EquivalenceGroups.empty ++ cleanPredicates.values.flatMap(_.map(_.freeTypeEquivalences))
      val predicateDecidedTypeData =
        decideTypes(
          cleanPredicates,
          typesEquivalences,
          typeData.structTypes.keys,
          Map.empty,
          (structId, argIndex) =>
            program.translationDirectives collectFirst {
              case TranslationDirective(scalaName, structId.name, args) if args.length == structId.arity => scalaName + "_" + args(argIndex).name
            } getOrElse (structId.name + "_" + structId.arity + "_arg" + argIndex)
        )
      val cleanStructs = typeData.structTypes.map { case (key, value) => (key, value.map(_.replaceArguments(cleanPredicates))) }
      val structDecidedTypeData =
        decideTypes(
          cleanStructs,
          typesEquivalences,
          typeData.structTypes.keys,
          predicateDecidedTypeData._2,
          (structId, argIndex) => structId.name + "_" + structId.arity + "_arg" + argIndex
        )

      (predicateDecidedTypeData._1, structDecidedTypeData._1, structDecidedTypeData._2)
    }
  }

  private def decideTypes(
                           types: StructTypeMap,
                           typeEquivalences: EquivalenceGroups[FreeType],
                           allStructs: Iterable[StructId],
                           traitMap: TraitMap,
                           argumentNameGenerator: (StructId, Int) => String
                         ): (Map[StructId, Seq[DecidedArgumentType]], TraitMap) =
    types.foldLeft[(Map[StructId, Seq[DecidedArgumentType]], TraitMap)]((Map.empty, traitMap))((mapFoldResult, currEntry) => {
      val valueTypeData =
        currEntry._2.zipWithIndex.foldLeft[(Seq[DecidedArgumentType], FreeTypeMap, TraitMap)]((Seq.empty, Map.empty, mapFoldResult._2))((typesFoldResult, currTypes) => {
          val ancestorData = currTypes._1.leastCommonAncestor(
            typesFoldResult._2,
            typeEquivalences,
            allStructs,
            typesFoldResult._3,
            argumentNameGenerator(currEntry._1, currTypes._2)
          )
          (typesFoldResult._1 :+ ancestorData._1, ancestorData._2, ancestorData._3)
        })
      (mapFoldResult._1 + (currEntry._1 -> valueTypeData._1), valueTypeData._3)
    })

  private def typesOfClause(clause: Clause): TranslationResult[ProgramTypeData] = {
    (clause.head +: clause.body.collect{case x: Struct => x}) translateMany typesOfPredicate map (mergeMany(_)) map { typeData =>
      var varMap = typeData.varTypes
      var nextToReplace = varMap.find(_._2.base exists (_.hasVariable))

      while (nextToReplace isDefined) {
        val replaceResult = nextToReplace.get._2.replaceVariables(varMap, nextToReplace.get._1)
        varMap = replaceResult._2 + (nextToReplace.get._1 -> replaceResult._1)
        nextToReplace = varMap.find(_._2.base exists (_.hasVariable))
      }

      ProgramTypeData(
        typeData.predicateTypes map { case (key, value) =>
          (key, value map (_.replaceVariables(varMap)))
        },
        typeData.structTypes map { case (key, value) =>
          (key, value map (_.replaceVariables(varMap)))
        }
      )
    }
  }

  private def typesOfPredicate(predicate: Struct): TranslationResult[ClauseTypeData] =
    predicate.args.zipWithIndex.translateManyWithContext(ClauseTypeData.empty)((predicateArgument, typeData) => predicateArgument._1 match {
      case Variable(name) =>
        TranslationResult.Success(
          (TypeOfVar(name), typeData mergeVariables  Map(name -> ArgumentTypeGroup(TypeOfArg(predicate.name, predicate.args.length, predicateArgument._2))))
        )
      case _ =>
        typeOfTerm(predicateArgument._1) map {case (argType, argTypeData) => (argType, typeData merge argTypeData)}
    }) map { case (argTypes, typesData) =>
      ClauseTypeData(Map(StructId(predicate.name, predicate.args.length) -> (argTypes map (ArgumentTypeGroup(_)))), typesData.structTypes, typesData.varTypes)
    }


  private def typeOfTerm(term: Term): TranslationResult[(ArgumentType, ClauseTypeData)] = term match {
    case Struct(name, args) =>
      args translateMany typeOfTerm map { translatedArgs =>
        (
          StructType(name, args.length),
          mergeMany(translatedArgs map(_._2)) mergeStructs Map(StructId(name, args.length) -> (translatedArgs map (_._1) map (ArgumentTypeGroup(_))))
        )
      }
    case ListTerm(values, tail) =>
      values translateMany typeOfTerm map { translatedValues =>
        val listType: ArgumentType = ListType(ArgumentTypeGroup(translatedValues map (_._1):_*))
        (
          listType,
          mergeMany(translatedValues map (_._2)) mergeVariables (tail map (v => Map(v.name -> ArgumentTypeGroup(listType))) getOrElse Map.empty)
        )
      }
    case Variable(name) =>
      TranslationResult.Success((TypeOfVar(name), ClauseTypeData.empty))
    case _ =>
      TranslationResult.Failure("Invalid term: " + term)
  }
}
