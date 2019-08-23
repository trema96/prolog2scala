package prolog2scala.translation.typing

import prolog2scala.translation.typing.DataMaps._
import prolog2scala.translation.typing.TypeData._

/**
  * Utility for merging multiple elements
  */
object Merge {
  def mergeMany[A](elements: Iterable[A])(implicit mergeFunction: (A, A) => A, emptyElement: A): A = elements.fold(emptyElement)(mergeFunction)

  implicit val mergeStructTypeMaps: (StructTypeMap, StructTypeMap) => StructTypeMap = _ merge _
  implicit val emptyStructTypeMap: StructTypeMap = Map.empty
  implicit val mergeVarTypeMaps: (VarTypeMap, VarTypeMap) => VarTypeMap = _ merge _
  implicit val emptyVarTypeMap: VarTypeMap = Map.empty
  implicit val mergeProgramTypeData: (ProgramTypeData, ProgramTypeData) => ProgramTypeData = _ merge _
  implicit val emptyProgramTypeData: ProgramTypeData = ProgramTypeData.empty
  implicit val mergeClauseTypeData: (ClauseTypeData, ClauseTypeData) => ClauseTypeData = _ merge _
  implicit val emptyClauseTypeData: ClauseTypeData = ClauseTypeData.empty
}
