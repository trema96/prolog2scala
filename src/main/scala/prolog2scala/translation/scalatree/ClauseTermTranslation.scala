package prolog2scala.translation.scalatree

sealed trait ClauseTermTranslation
import treehugger.forest._
import treehuggerDSL._

object ClauseTermTranslation {
  case class ForElement(node: Enumerator) extends ClauseTermTranslation
  case object Cut extends ClauseTermTranslation
  case class Condition(node: IfStart) extends ClauseTermTranslation
}
