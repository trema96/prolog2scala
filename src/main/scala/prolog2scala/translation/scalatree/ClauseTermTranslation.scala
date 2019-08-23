package prolog2scala.translation.scalatree

import treehugger.forest._
import treehuggerDSL._

/**
  * Holds data useful for the translation of a clause term
  */
sealed trait ClauseTermTranslation

object ClauseTermTranslation {
  case class ForElement(node: Enumerator) extends ClauseTermTranslation
  case object Cut extends ClauseTermTranslation
  case class Condition(node: IfStart) extends ClauseTermTranslation
}
