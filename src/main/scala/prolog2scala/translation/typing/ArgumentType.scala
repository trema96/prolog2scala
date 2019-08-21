package prolog2scala.translation.typing

sealed trait ArgumentType {
  def hasVariable: Boolean
}

object ArgumentType {
  case class ListType(elemType: ArgumentTypeGroup) extends ArgumentType {
    override def hasVariable: Boolean = elemType.base exists (_.hasVariable)
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
