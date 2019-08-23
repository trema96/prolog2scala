package prolog2scala.translation.typing

import prolog2scala.translation.{EquivalenceGroups, StructId, Utils}
import prolog2scala.translation.typing.ArgumentType._
import prolog2scala.translation.typing.DataMaps._

/**
  * Group containing all the types some prolog element was used with
  */
class ArgumentTypeGroup private (val base: Set[ArgumentType]) {
  /**
    * @return a new group containing also the types from the other one
    */
  def ++(other: ArgumentTypeGroup): ArgumentTypeGroup = ArgumentTypeGroup(base ++ other.base)

  /**
    * @return a new group containing also the provided type
    */
  def +(typeToAdd: ArgumentType): ArgumentTypeGroup = ArgumentTypeGroup(base + typeToAdd)

  /**
    * Replaces once all occurrences of variables in this group, according to the provided map
    * @param replaceMap what each variable should be replaced with
    * @param noReplace variables with this name will be only removed and not replaced
    * @return a new group with the variables replaced and the updated variable map
    */
  def replaceVariables(replaceMap: VarTypeMap, noReplace: String): (ArgumentTypeGroup, VarTypeMap) = replaceVariables(replaceMap, Some(noReplace))
  /**
    * Replaces once all occurrences of variables in this group, according to the provided map
    * @param replaceMap what each variable should be replaced with
    * @return a new group with the variables replaced
    */
  def replaceVariables(replaceMap: VarTypeMap): ArgumentTypeGroup = replaceVariables(replaceMap, None)._1
  /**
    * Recursivelty replaces all occurrences of argument references in this group, according to the provided map
    * @param replaceMap what each argument reference should be replaced with
    * @param noReplaceName reference to argument with (this predicate name, provided arity, provided index) will be only removed and not replaced
    * @param noReplaceArity reference to argument with (provided predicate name, this arity, provided index) will be only removed and not replaced
    * @param noReplaceArgIndex reference to argument with (provided predicate name, provided arity, this index) will be only removed and not replaced
    * @return a new group with the argument reference replaced and the updated argument map
    */
  def replaceArguments(replaceMap: StructTypeMap, noReplaceName: String, noReplaceArity: Int, noReplaceArgIndex: Int): (ArgumentTypeGroup, StructTypeMap) =
    replaceArguments(replaceMap, Set((noReplaceName, noReplaceArity, noReplaceArgIndex)))
  /**
    * Recursivelty replaces all occurrences of argument references in this group, according to the provided map
    * @param replaceMap what each argument reference should be replaced with
    * @return a new group with the argument reference replaced
    */
  def replaceArguments(replaceMap: StructTypeMap): ArgumentTypeGroup = replaceArguments(replaceMap, Set())._1
  /**
    * Groups free types. Also does this on list inner types
    * @return The equivalence groups of free types in this group
    */
  def freeTypeEquivalences: EquivalenceGroups[FreeType] =
    EquivalenceGroups.empty + base.collect{case x: FreeType => x} ++ base.collect{case ListType(types) => types.freeTypeEquivalences}
  /**
    * Decides what type is better suited for the entire group
    * @param freeTypeMap all known mappings from free type group representative to type argument of this scope
    * @param freeTypeEq all known free type equivalence groups
    * @param allStructs all known structs
    * @param traitMap all knwon traits
    * @param argName the name of the argument this group refers to, used when generating new trait
    * @return (the type more suitable to represent this group, updated free type map, updated trait map)
    */
  def leastCommonAncestor(
                           freeTypeMap: FreeTypeMap,
                           freeTypeEq: EquivalenceGroups[FreeType],
                           allStructs: Iterable[StructId],
                           traitMap: TraitMap,
                           argName: String
                         ): (DecidedArgumentType, FreeTypeMap, TraitMap) = {
    require(base.nonEmpty, "leastCommonAncestor can be obtained only from a non-empty type set")

    if (base.forall(_.isInstanceOf[FreeType])) {
      val typeArg = getTypeArg(base.head.asInstanceOf[FreeType], freeTypeMap, freeTypeEq)
      (typeArg._1, typeArg._2, traitMap)
    } else {
      val noFreeTypes = base.filter(!_.isInstanceOf[FreeType])
      if (noFreeTypes.forall(_.isInstanceOf[StructType])) {
        val structType = getStructType(noFreeTypes map (_.asInstanceOf[StructType]), allStructs, traitMap, argName)
        (structType._1, freeTypeMap, structType._2)
      } else if (noFreeTypes.forall(_.isInstanceOf[ListType])) {
        val elemTypeData = (noFreeTypes collect {
          case ListType(elemType) => elemType
        }).fold(ArgumentTypeGroup.empty)(_ ++ _).leastCommonAncestor(freeTypeMap, freeTypeEq, allStructs, traitMap, argName)

        (DecidedArgumentType.ListType(elemTypeData._1), elemTypeData._2, elemTypeData._3)
      } else {
        (DecidedArgumentType.AnyType, freeTypeMap, Map.empty)
      }
    }
  }

  private def replaceVariables(replaceMap: VarTypeMap, noReplace: Option[String]): (ArgumentTypeGroup, VarTypeMap) =
    base.foldLeft((ArgumentTypeGroup.empty, replaceMap))((updatedValues, current) => current match {
      case TypeOfVar(varName) if noReplace.isEmpty || varName != noReplace.get =>
        if (updatedValues._2 contains varName) {
          (updatedValues._1 ++ updatedValues._2(varName), updatedValues._2)
        } else {
          val newType = FreeType.generateNew
          (updatedValues._1 + newType, updatedValues._2 + (varName -> ArgumentTypeGroup(newType)))
        }
      case TypeOfVar(_) =>
        updatedValues
      case ListType(elemType) =>
        val elemTypeReplaceRes = elemType.replaceVariables(updatedValues._2, noReplace)
        (updatedValues._1 + ListType(elemTypeReplaceRes._1), elemTypeReplaceRes._2)
      case x => (updatedValues._1 + x, updatedValues._2)
    })

  private def replaceArguments(replaceMap: StructTypeMap, noReplace: Set[(String, Int, Int)]): (ArgumentTypeGroup, StructTypeMap) = {
    val res: (ArgumentTypeGroup, StructTypeMap) = base.foldLeft((ArgumentTypeGroup.empty, replaceMap))((updatedValues, current) => current match {
      case ListType(elemType) =>
        val replaceRes = elemType.replaceArguments(updatedValues._2, noReplace)
        (updatedValues._1 + ListType(replaceRes._1), replaceRes._2)
      case TypeOfArg(predName, predArity, argIndex) if noReplace.contains((predName, predArity, argIndex)) =>
        updatedValues
      case TypeOfArg(predName, predArity, argIndex) =>
        updatedValues._2
          .get(StructId(predName, predArity))
          .map(_ (argIndex))
          .map(_.replaceArguments(updatedValues._2, noReplace + ((predName, predArity, argIndex))))
          .map{ replaceRes =>
            (
              updatedValues._1 ++ replaceRes._1,
              updatedValues._2 + (StructId(predName, predArity) -> updatedValues._2(StructId(predName, predArity)).updated(argIndex, replaceRes._1))
            )
          }
          .getOrElse(updatedValues)
      case x =>
        (updatedValues._1 + x, updatedValues._2)
    })

    if (res._1.base isEmpty) (ArgumentTypeGroup(FreeType.generateNew), res._2) else res
  }

  private def getTypeArg(tp: FreeType, freeTypeMap: FreeTypeMap, freeTypeEq: EquivalenceGroups[FreeType]): (DecidedArgumentType.TypeArg,FreeTypeMap) = {
    val groupRep = freeTypeEq.find(tp)
    freeTypeMap get groupRep map {i =>
      (DecidedArgumentType.TypeArg(i), freeTypeMap)
    } getOrElse {
      val newIndex = freeTypeMap.size
      (
        DecidedArgumentType.TypeArg(newIndex),
        freeTypeMap  + (groupRep -> newIndex),
      )
    }
  }

  private def getStructType(types: Set[StructType], allStructs: Iterable[StructId], traitMap: TraitMap, argName: String): (DecidedArgumentType, TraitMap) = {
    if (types.size == 1) {
      (
        DecidedArgumentType.StructType(Utils.structToScalaName(StructId(types.head.structName, types.head.structArgsCount), allStructs)),
        traitMap
      )
    } else {
      traitMap get types map { tp =>
        (tp, traitMap)
      } getOrElse {
        val newType = DecidedArgumentType.GroupType(Utils.structNameToScala(argName))
        (
          newType,
          traitMap + (types -> newType)
        )
      }
    }
  }
}

object ArgumentTypeGroup {
  def apply(types: Set[ArgumentType]): ArgumentTypeGroup = {
    val listTypes: Set[ArgumentTypeGroup] = types.collect{case ListType(elemType) => elemType}
    new ArgumentTypeGroup(
      types.filter(!_.isInstanceOf[ListType]) ++ (if (listTypes nonEmpty) Set(ListType(listTypes.fold(ArgumentTypeGroup.empty)(_ ++ _))) else Set())
    )
  }
  def apply(types: ArgumentType*): ArgumentTypeGroup = ArgumentTypeGroup(Set(types:_*))
  def empty: ArgumentTypeGroup = ArgumentTypeGroup()
}