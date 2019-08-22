package prolog2scala.translation

import treehugger.forest._
import treehuggerDSL._
import definitions._

object TreeHuggerUtils {
  def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
  def toEmptyClassDef(classDef: ClassDefStart): ClassDef = classDef.mkTree(EmptyTree)
  def flattenedTypeTuple(types: Iterable[Type]): Type =
  if (types isEmpty)
    UnitClass
  else if (types.size == 1)
    types.head
  else
    TYPE_TUPLE(types)
}
