package prolog2scala.translation

import treehugger.forest._
import definitions._
import treehugger.forest
import treehuggerDSL._

sealed trait DecidedArgumentType {
  def treeType: Type
  def typeArg: Option[DecidedArgumentType.TypeArg]
}

object DecidedArgumentType {
  case object AnyType extends DecidedArgumentType {
    override def treeType: forest.Type = AnyClass
    override def typeArg: Option[TypeArg] = None
  }
  case class ListType(argumentType: DecidedArgumentType) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_LIST(argumentType.treeType)
    override def typeArg: Option[TypeArg] = argumentType.typeArg
  }
  case class TypeArg(index: Int) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(typeName)
    override def typeArg: Option[TypeArg] = Some(this)
    def typeDef: forest.TypeDef = TYPEVAR(typeName)
    private def typeName = "A" + (index + 1)
  }
  case class StructType(name: String) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(name)
    override def typeArg: Option[TypeArg] = None
  }
  case class GroupType(name: String) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(name)
    override def typeArg: Option[TypeArg] = None
  }
}
