package prolog2scala.translation.typing

import DataMaps._

/**
  * Data necessary for program typing
  */
object TypeData {
  sealed trait BaseTypeData {
    val predicateTypes: StructTypeMap
    val structTypes: StructTypeMap

    def mergePredicates(other: StructTypeMap): SelfType = build(predicateTypes merge other, structTypes)
    def mergeStructs(other: StructTypeMap): SelfType = build(predicateTypes, structTypes merge other)
    def merge(other: SelfType): SelfType

    protected type SelfType
    protected def build(newPreds: StructTypeMap, newStructs: StructTypeMap): SelfType
  }

  case class ProgramTypeData(override val predicateTypes: StructTypeMap, override val structTypes: StructTypeMap) extends BaseTypeData {
    override def merge(other: ProgramTypeData): ProgramTypeData = ProgramTypeData(predicateTypes merge other.predicateTypes, structTypes merge other.structTypes)

    override protected type SelfType = ProgramTypeData
    override protected def build(newPreds: StructTypeMap, newStructs: StructTypeMap): ProgramTypeData = ProgramTypeData(newPreds, newStructs)
  }

  object ProgramTypeData {
    val empty: ProgramTypeData = ProgramTypeData(Map.empty, Map.empty)
  }

  case class ClauseTypeData(override val predicateTypes: StructTypeMap, override val structTypes: StructTypeMap, varTypes: VarTypeMap) extends BaseTypeData {
    override def merge(other: ClauseTypeData): ClauseTypeData =
      ClauseTypeData(predicateTypes merge other.predicateTypes, structTypes merge other.structTypes, varTypes merge other.varTypes)
    def mergeVariables(other: VarTypeMap): ClauseTypeData = ClauseTypeData(predicateTypes, structTypes, varTypes merge other)

    override protected type SelfType = ClauseTypeData
    override protected def build(newPreds: StructTypeMap, newStructs: StructTypeMap): ClauseTypeData = ClauseTypeData(newPreds, newStructs, varTypes)
  }

  object ClauseTypeData {
    val empty: ClauseTypeData = ClauseTypeData(Map.empty, Map.empty, Map.empty)
  }
}