package prolog2scala.translation.typing

import prolog2scala.translation.{DecidedArgumentType, EquivalenceGroups, StructId, Utils}
import prolog2scala.translation.typing.ArgumentType._
import prolog2scala.translation.typing.DataMaps._

class ArgumentTypeGroup private (val base: Set[ArgumentType]) {
  //TODO docs
  def merge(other: ArgumentTypeGroup): ArgumentTypeGroup = ArgumentTypeGroup(base ++ other.base)
  def +(typeToAdd: ArgumentType): ArgumentTypeGroup = ArgumentTypeGroup(base + typeToAdd)
  def replaceVariables(replaceMap: VarTypeMap, noReplace: String): (ArgumentTypeGroup, VarTypeMap) = replaceVariables(replaceMap, Some(noReplace))
  def replaceVariables(replaceMap: VarTypeMap): ArgumentTypeGroup = replaceVariables(replaceMap, None)._1
  def replaceArguments(replaceMap: StructTypeMap, noReplaceName: String, noReplaceArity: Int, noReplaceArgIndex: Int): (ArgumentTypeGroup, StructTypeMap) =
    replaceArguments(replaceMap, Set((noReplaceName, noReplaceArity, noReplaceArgIndex)))
  def replaceArguments(replaceMap: StructTypeMap): ArgumentTypeGroup = replaceArguments(replaceMap, Set())._1
  def freeTypeEquivalences: EquivalenceGroups[FreeType] =
    EquivalenceGroups.empty.union(base.collect{case x: FreeType => x}) merge base.collect{case ListType(types) => types.freeTypeEquivalences}

  private def replaceVariables(replaceMap: VarTypeMap, noReplace: Option[String]): (ArgumentTypeGroup, VarTypeMap) =
    base.foldLeft((ArgumentTypeGroup.empty, replaceMap))((updatedValues, current) => current match {
      case TypeOfVar(varName) if noReplace.isEmpty || varName != noReplace.get =>
        if (updatedValues._2 contains varName) {
          (updatedValues._1 merge updatedValues._2(varName), updatedValues._2)
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
              updatedValues._1 merge replaceRes._1,
              updatedValues._2 + (StructId(predName, predArity) -> updatedValues._2(StructId(predName, predArity)).updated(argIndex, replaceRes._1))
            )
          }
          .getOrElse(updatedValues)
      case x =>
        (updatedValues._1 + x, updatedValues._2)
    })

    if (res._1.base isEmpty) (ArgumentTypeGroup(FreeType.generateNew), res._2) else res
  }

  def leastCommonAncestor(
                           freeTypeMap: FreeTypeMap,
                           freeTypeEq: EquivalenceGroups[FreeType],
                           structTypeMap: StructTypeMap,
                           traitMap: TraitMap,
                           argName: String
                         ): (DecidedArgumentType,FreeTypeMap, TraitMap) = {
    require(base.nonEmpty, "leastCommonAncestor can be obtained only from a non-empty type set")
    //TODO still needs improvement

    if (base.forall(_.isInstanceOf[FreeType])) {
      val groupRep = freeTypeEq.find(base.head.asInstanceOf[FreeType])
      freeTypeMap get groupRep map {i =>
        (DecidedArgumentType.TypeArg(i), freeTypeMap, traitMap)
      } getOrElse {
        val newIndex = freeTypeMap.getNextIndex
        (
          DecidedArgumentType.TypeArg(newIndex),
          freeTypeMap  + (groupRep -> newIndex),
          traitMap
        )
      }
    } else {
      val noFreeTypes = base.filter(!_.isInstanceOf[FreeType])
      if (noFreeTypes.forall(_.isInstanceOf[StructType])) {
        if (noFreeTypes.size == 1) {
          val resTypeData = noFreeTypes.head.asInstanceOf[StructType]
          (
            DecidedArgumentType.StructType(Utils.structNameToScala(
              if (structTypeMap.keys.count(_.name == resTypeData.structName) > 1) {
                resTypeData.structName + "_" + resTypeData.structArgsCount
              } else {
                resTypeData.structName
              }
            )),
            freeTypeMap,
            traitMap
          )
        } else {
          val structs = noFreeTypes.map(_.asInstanceOf[StructType])
          traitMap get structs map { tp =>
            (tp, freeTypeMap, traitMap)
          } getOrElse {
            val newType = DecidedArgumentType.GroupType(Utils.structNameToScala(argName))
            (
              newType,
              freeTypeMap,
              traitMap + (structs -> newType)
            )
          }
        }
      } else if (noFreeTypes.forall(_.isInstanceOf[ListType])) {
        val elemTypeData = (noFreeTypes collect {
          case ListType(elemType) => elemType
        }).fold(ArgumentTypeGroup.empty)(_ merge _).leastCommonAncestor(freeTypeMap, freeTypeEq, structTypeMap, traitMap, argName)

        (DecidedArgumentType.ListType(elemTypeData._1), elemTypeData._2, elemTypeData._3)
      } else {
        (DecidedArgumentType.AnyType, freeTypeMap, Map.empty)
      }
    }
  }

  implicit class FreeTypeMapExt(base: FreeTypeMap) {
    def getNextIndex: Int = base.size
  }
}

object ArgumentTypeGroup {
  def apply(types: Set[ArgumentType]): ArgumentTypeGroup = {
    val listTypes: Set[ArgumentTypeGroup] = types.collect{case ListType(elemType) => elemType}
    new ArgumentTypeGroup(
      types.filter(!_.isInstanceOf[ListType]) ++ (if (listTypes nonEmpty) Set(ListType(listTypes.fold(ArgumentTypeGroup.empty)(_ merge _))) else Set())
    )
  }
  def apply(types: ArgumentType*): ArgumentTypeGroup = ArgumentTypeGroup(Set(types:_*))
  def empty: ArgumentTypeGroup = ArgumentTypeGroup()
}