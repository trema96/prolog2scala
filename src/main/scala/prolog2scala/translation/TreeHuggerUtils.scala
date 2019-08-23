package prolog2scala.translation

import treehugger.forest._
import treehuggerDSL._
import definitions._

object TreeHuggerUtils {
  /**
    * Finalizes the provided parameter declaration
    * @param param A parameter node
    * @return the finalized parameter node, that can be used as a def parameter
    */
  def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
  /**
    * Finalizes the provided class declaration
    * @param classDef A class definition
    * @return the finalized class tree, with no block
    */
  def toEmptyClassDef(classDef: ClassDefStart): ClassDef = classDef.mkTree(EmptyTree)

  /**
    * Creates a single type from the provided types
    * @param types types to be flattened
    * @return the unit class if the provided types were empty, a single type if there was only one type, or a tuple with all the provided types
    */
  def flattenedTypeTuple(types: Iterable[Type]): Type =
  if (types isEmpty)
    UnitClass
  else if (types.size == 1)
    types.head
  else
    TYPE_TUPLE(types)
}
