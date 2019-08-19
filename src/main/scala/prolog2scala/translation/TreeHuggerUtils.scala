package prolog2scala.translation

import treehugger.forest._
import treehuggerDSL._

object TreeHuggerUtils {
  def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
  def toEmptyClassDef(classDef: ClassDefStart): ClassDef = classDef.mkTree(EmptyTree)
}
