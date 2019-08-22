package prolog2scala.translation

import prolog2scala.translation.scalatree.ProgramTranslation
import prolog2scala.translation.typing.ProgramTyping
import treehugger.forest._
import treehuggerDSL._

case class Program(translationDirectives: Seq[TranslationDirective], predicates: Map[StructId, Seq[Clause]]) {
  def translate(scalaModuleName: String = "TranslatedProgram"): TranslationResult[String] =
    ProgramTyping.typesOf(this) flatMap {typeData =>
      ProgramTranslation.treeOf(this, typeData._1) map { predicateDefs =>
        OBJECTDEF(scalaModuleName) := BLOCK(typeData._2 ++ predicateDefs)
      }
    } map (treeToString(_))
}