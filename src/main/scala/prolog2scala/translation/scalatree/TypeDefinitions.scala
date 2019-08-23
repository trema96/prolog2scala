package prolog2scala.translation.scalatree

import treehugger.forest
import forest._
import treehuggerDSL._

/**
  * Types used in the program tree
  */
object TypeDefinitions {
  def PREDICATE(inputType: Type, outputType: Type): forest.Tree = REF("Predicate") APPLYTYPE (inputType, outputType)
  def TYPE_STREAM(elemType: Type): Type = TYPE_REF("Stream") TYPE_OF elemType
  def FACT(expression: CaseDef): Tree = REF("Fact") APPLY BLOCK(expression)
  def RULE(expression: CaseDef): Tree = REF("Rule") APPLY BLOCK(expression)
}
