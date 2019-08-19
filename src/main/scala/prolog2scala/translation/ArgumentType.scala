package prolog2scala.translation

sealed trait ArgumentType {
  def hasVariable: Boolean
}

object ArgumentType {
  case class ListType(elemType: Set[ArgumentType]) extends ArgumentType {
    override def hasVariable: Boolean = elemType exists (_.hasVariable)
  }
  case class StructType(structName: String, structArgsCount: Int) extends ArgumentType {
    override def hasVariable: Boolean = false
  }
  case class TypeOfVar(varName: String) extends ArgumentType {
    override def hasVariable: Boolean = true
  }
  case class TypeOfArg(predicateName: String, predicateArity: Int, argIndex: Int) extends ArgumentType {
    override def hasVariable: Boolean = false
  }
  class FreeType private(private val id: Int) extends ArgumentType {
    override def hasVariable: Boolean = false

    def canEqual(other: Any): Boolean = other.isInstanceOf[FreeType]

    override def equals(other: Any): Boolean = other match {
      case that: FreeType =>
        (that canEqual this) &&
          id == that.id
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(id)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }
  object FreeType {
    private var id: Int = 0
    def generateNew: FreeType = {id += 1; new FreeType(id - 1)}
  }
}

import treehugger.forest._
import definitions._
import treehugger.forest
import treehuggerDSL._

sealed trait DecidedArgumentType {
  def treeType: Type
}

object DecidedArgumentType {
  case object AnyType extends DecidedArgumentType {
    override def treeType: forest.Type = AnyClass
  }
  case class ListType(argumentType: DecidedArgumentType) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_LIST(argumentType.treeType)
  }
  case class TypeArg(index: Int) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(typeName)
    def typeDef: TypeDef = TYPEVAR(typeName)
    private def typeName = "A" + (index + 1)
  }
  case class StructType(name: String) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(name)
  }
  case class GroupType(name: String) extends DecidedArgumentType {
    override def treeType: forest.Type = TYPE_REF(name)
  }
}
