package prolog2scala.translation.typing

import prolog2scala.translation.typing.ArgumentType.{FreeType, StructType}
import prolog2scala.translation.StructId

import scala.language.reflectiveCalls

object DataMaps {
  /**
    * struct => argumentAssociatedTypes
    */
  type StructTypeMap = Map[StructId, Seq[ArgumentTypeGroup]]

  implicit class RichStructTypeMap(base: StructTypeMap) {
    def merge(other: StructTypeMap): StructTypeMap = mergeMap(base, other)((valA, valB) =>
      valA zip valB map {case (a, b) => a ++ b}
    )
  }

  /**
    * varName => associatedTypes
    */
  type VarTypeMap = Map[String, ArgumentTypeGroup]

  implicit class RichVarTypeMap(base: VarTypeMap) {
    def merge(other: VarTypeMap): VarTypeMap = mergeMap(base, other)((a, b) => a ++ b)
  }

  implicit class ManyVarTypeMap(base: Iterable[StructTypeMap]) {
    def mergeMany: StructTypeMap = base.fold(Map.empty)(_ merge _)
  }

  /**
    * freeTypeGroupRepresentative => typeArgumentIndex
    */
  type FreeTypeMap = Map[FreeType, Int]
  /**
    * structGroup => commonTrait
    */
  type TraitMap = Map[Set[StructType], DecidedArgumentType.GroupType]

  private def mergeMap[A, B](a: Map[A, B], b: Map[A, B])(valueMerge: (B, B) => B): Map[A, B] =
    a.foldLeft(b)((merged, entry) =>
      if (merged.contains(entry._1))
        merged + (entry._1 -> valueMerge(merged(entry._1), entry._2))
      else
        merged + entry
    )
}

