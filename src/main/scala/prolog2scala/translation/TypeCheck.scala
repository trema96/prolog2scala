package prolog2scala.translation

import TypeCheck._
import prolog2scala.translation.ArgumentType._
import prolog2scala.translation.Utils._

case class ClauseTermTypeCheckContext(structs: StructTypeMap, vars: VarTypeMap) {
  def join(other: ClauseTermTypeCheckContext): ClauseTermTypeCheckContext = ClauseTermTypeCheckContext(structs join other.structs, vars join other.vars)
  def joinStruct(structTypeMap: StructTypeMap): ClauseTermTypeCheckContext = ClauseTermTypeCheckContext(structs join structTypeMap, vars)
  def joinVar(varTypeMap: VarTypeMap): ClauseTermTypeCheckContext = ClauseTermTypeCheckContext(structs, vars join varTypeMap)
}

object ClauseTermTypeCheckContext {
  def empty: ClauseTermTypeCheckContext = ClauseTermTypeCheckContext(Map.empty, Map.empty)
}

case class ClauseTypeCheckContext(predicates: PredicateTypeMap, structs: StructTypeMap, vars: VarTypeMap) {
  def join(other: ClauseTypeCheckContext): ClauseTypeCheckContext = ClauseTypeCheckContext(predicates join other.predicates, structs join other.structs, vars join other.vars)
  def joinPredicate(predicateTypeMap: PredicateTypeMap): ClauseTypeCheckContext = ClauseTypeCheckContext(predicates join predicateTypeMap, structs, vars)
  def joinStruct(structTypeMap: StructTypeMap): ClauseTypeCheckContext = ClauseTypeCheckContext(predicates, structs join structTypeMap, vars)
  def joinVar(varTypeMap: VarTypeMap): ClauseTypeCheckContext = ClauseTypeCheckContext(predicates, structs, vars join varTypeMap)
}

object ClauseTypeCheckContext {
  def empty: ClauseTypeCheckContext = ClauseTypeCheckContext(Map.empty, Map.empty, Map.empty)
}

case class TypeCheckContext(predicates: PredicateTypeMap, structs: StructTypeMap) {
  def join(other: TypeCheckContext): TypeCheckContext = TypeCheckContext(predicates join other.predicates, structs join other.structs)
}

object TypeCheckContext {
  def empty: TypeCheckContext = TypeCheckContext(Map.empty, Map.empty)
}

object TypeCheck {
  //(predicateName, predicateArgumentsNumber, argumentIndex) => argumentTypes
  type PredicateTypeMap = Map[(String, Int), Seq[Set[ArgumentType]]]
  //(structName, structArgumentsNumber) => argumentTypes
  type StructTypeMap = Map[(String, Int), Seq[Set[ArgumentType]]]
  //varName => varTypes
  type VarTypeMap = Map[String, Set[ArgumentType]]
  //freeType => typeArgName
  type FreeTypeMap = Map[FreeType, Int]
  //struct => structTraits
  type TraitMap = Map[Set[StructType], DecidedArgumentType.GroupType]


  //TODO generalizzabile con superclass
  implicit class StructTypeMapExt(base: StructTypeMap) {
    def join(other: StructTypeMap): StructTypeMap = base.foldLeft(other)((allMap, elem) =>
      if (allMap contains elem._1) {
        allMap + (elem._1 -> (allMap(elem._1) zip elem._2 map (v => v._1 join v._2)))
      } else {
        allMap + elem
      }
    )
  }

  implicit class ManyStructTypeMapExt(base: Iterable[StructTypeMap]) {
    def joinMany: StructTypeMap = base.fold(Map.empty)(_ join _)
  }

  implicit class VarTypeMapExt(base: VarTypeMap) {
    def join(other: VarTypeMap): VarTypeMap = base.foldLeft(other)((allMap, elem) =>
      if (allMap contains elem._1) {
        allMap + (elem._1 -> (allMap(elem._1) join elem._2))
      } else {
        allMap + elem
      }
    )
  }

  implicit class ManyVarTypeMapExt(base: Iterable[VarTypeMap]) {
    def joinMany: VarTypeMap = base.fold(Map.empty)(_ join _)
  }

  implicit class ManyClauseTermTypeCheckContextExt(base: Iterable[ClauseTermTypeCheckContext]) {
    def joinMany: ClauseTermTypeCheckContext = base.fold(ClauseTermTypeCheckContext.empty)(_ join _)
  }
  implicit class ManyClauseTypeCheckContextExt(base: Iterable[ClauseTypeCheckContext]) {
    def joinMany: ClauseTypeCheckContext = base.fold(ClauseTypeCheckContext.empty)(_ join _)
  }
  implicit class ManyTypeCheckContextExt(base: Iterable[TypeCheckContext]) {
    def joinMany: TypeCheckContext = base.fold(TypeCheckContext.empty)(_ join _)
  }

  implicit class ArgumentTypeSetExt(base: Set[ArgumentType]) {
    private def replaceVariables(replaceMap: VarTypeMap, noReplace: Option[String]): (Set[ArgumentType], VarTypeMap) =
      base.foldLeft[(Set[ArgumentType], VarTypeMap)]((Set.empty, replaceMap))((updatedValues, current) => current match {
        case TypeOfVar(varName) if noReplace.isEmpty || varName != noReplace.get =>
          if (updatedValues._2 contains varName) {
            (updatedValues._1 ++ updatedValues._2(varName), updatedValues._2)
          } else {
            val newType = FreeType.generateNew
            (updatedValues._1 + newType, updatedValues._2 + (varName -> Set(newType)))
          }
        case TypeOfVar(_) => updatedValues
        case ListType(elemType) =>
          val elemTypeReplaceRes = elemType.replaceVariables(updatedValues._2, noReplace)
          (updatedValues._1 + ListType(elemTypeReplaceRes._1), elemTypeReplaceRes._2)
        case x => (updatedValues._1 + x, updatedValues._2)
      })

    def replaceVariables(replaceMap: VarTypeMap, noReplace: String): (Set[ArgumentType], VarTypeMap) = replaceVariables(replaceMap, Some(noReplace))
    def replaceVariables(replaceMap: VarTypeMap): Set[ArgumentType] = replaceVariables(replaceMap, None)._1

    private def replaceArguments(replaceMap: PredicateTypeMap, noReplace: Set[(String, Int, Int)]): (Set[ArgumentType], PredicateTypeMap) = {
      val res = base.foldLeft[(Set[ArgumentType], PredicateTypeMap)]((Set.empty, replaceMap))((updatedValues, current) => current match {
        case ListType(elemType) =>
          val replaceRes = elemType.replaceArguments(updatedValues._2, noReplace)
          (updatedValues._1 + ListType(replaceRes._1), replaceRes._2)
        case TypeOfArg(predName, predArity, argIndex) if noReplace.contains((predName, predArity, argIndex)) =>
          updatedValues
        case TypeOfArg(predName, predArity, argIndex) =>
          updatedValues._2
            .get((predName, predArity))
            .map(_ (argIndex))
            .map(_.replaceArguments(updatedValues._2, noReplace + ((predName, predArity, argIndex))))
            .map{ replaceRes =>
              (updatedValues._1 ++ replaceRes._1, updatedValues._2 + ((predName, predArity) -> updatedValues._2((predName, predArity)).updated(argIndex, replaceRes._1)))
            }
            .getOrElse((updatedValues._1, updatedValues._2))
        case x =>
          (updatedValues._1 + x, updatedValues._2)
      })
      if (res._1 isEmpty) (Set(FreeType.generateNew), res._2) else res
    }
    def replaceArguments(replaceMap: PredicateTypeMap, noReplaceName: String, noReplaceArity: Int, noReplaceArgIndex: Int): (Set[ArgumentType], PredicateTypeMap) =
      replaceArguments(replaceMap, Set((noReplaceName, noReplaceArity, noReplaceArgIndex)))
    def replaceArguments(replaceMap: PredicateTypeMap): Set[ArgumentType] = replaceArguments(replaceMap, Set())._1

    def freeTypeEquivalences: EquivalenceGroups[FreeType] =
      EquivalenceGroups.empty.union(base.collect{case x: FreeType => x}) joinMany base.collect{case ListType(types) => types.freeTypeEquivalences}

    def leastCommonAncestor(
                             freeTypeMap: FreeTypeMap,
                             freeTypeEq: EquivalenceGroups[FreeType],
                             structTypeMap: StructTypeMap,
                             traitMap: TraitMap,
                             argName: String
                           ): (DecidedArgumentType,FreeTypeMap, TraitMap) = {
      require(base.nonEmpty, "leastCommonAncestor can be obtained only from a non-empty type set")

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
              DecidedArgumentType.StructType(structToScalaName(resTypeData, structTypeMap.keys)),
              freeTypeMap,
              traitMap
            )
          } else {
            val structs = noFreeTypes.map(_.asInstanceOf[StructType])
            traitMap get structs map { tp =>
              (tp, freeTypeMap, traitMap)
            } getOrElse {
              val newType = DecidedArgumentType.GroupType(structNameToScala(argName))
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
          }).fold(Set.empty)(_ ++ _).leastCommonAncestor(freeTypeMap, freeTypeEq, structTypeMap, traitMap, argName)

          (DecidedArgumentType.ListType(elemTypeData._1), elemTypeData._2, elemTypeData._3)
        } else {
          (DecidedArgumentType.AnyType, freeTypeMap, Map.empty)
        }
      }
    }

    def join(other: Set[ArgumentType]): Set[ArgumentType] = {
      val baseListTypes = base.collect{case x: ListType => x}
      val otherListTypes = other.collect{case x: ListType => x}
      val jointListTypes = (baseListTypes ++ otherListTypes).map(_.elemType).fold(Set.empty)(_ join _)
      (base.filter(!_.isInstanceOf[ListType]) ++ other.filter(!_.isInstanceOf[ListType])) ++ (if (jointListTypes isEmpty) Set() else Set(ListType(jointListTypes)))
    }
  }

  implicit class FreeTypeMapExt(base: FreeTypeMap) {
    def getNextIndex: Int = base.size
  }

  def structToScalaName(struct: ArgumentType.StructType, allStructs: Iterable[(String, Int)]): String =
    structNameToScala(
      if (allStructs.count(_._1 == struct.structName) > 1) {
        struct.structName + "_" + struct.structArgsCount
      } else {
        struct.structName
      }
    )
}
