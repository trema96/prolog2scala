package prolog2scala.translation

import org.scalatest.{FlatSpec, Matchers}
import prolog2scala.translation.parsing.ParsingRules
import fastparse._
import prolog2scala.translation.PredicateArgument.Direction.{In, Out}
import prolog2scala.translation.Term._
import prolog2scala.translation.TestPrograms.{ProgramData, ProgramString}

trait ProgramBehaviour { this: FlatSpec with Matchers =>
  def correctProgram(programData: => ProgramData) {
    it should "be parsed correctly" in {
      Program.parse(programData.program) should matchPattern {
        case Parsed.Success(_, _) =>
      }
    }

    it should "be translated correctly" in {
      val Parsed.Success(programTree, _) = Program.parse(programData.program)
      programTree.translate() should matchPattern {
        case TranslationResult.Success(_) =>
      }
    }

    it should "match expected translation" in {
      val Parsed.Success(programTree, _) = Program.parse(programData.program)
      val TranslationResult.Success(scalaCode) = programTree.translate()
      val actual = scalaCode.toCharArray.map(_.toInt).filter(_ != 13).map(_.toChar).mkString("")
      val expected = programData.expectedTranslation
      actual shouldEqual expected
    }
  }

  def correctProgramNonTranslatable(program: => String): Unit = {
    it should "be parsed correctly" in {
      Program.parse(program) should matchPattern {
        case Parsed.Success(_, _) =>
      }
    }

    it should "not be translated correctly" in {
      val Parsed.Success(programTree, _) = Program.parse(program)
      programTree.translate() should matchPattern {
        case TranslationResult.Failure(_) =>
      }
    }
  }
}

class TestProgramTranslation extends FlatSpec with Matchers with ProgramBehaviour {
  TestPrograms.correctProgramsData foreach { programData =>
    s"Correct program ${programData.programName}" should behave like correctProgram(programData)
  }

  "A correct program parsing result" should "match the expected tree" in {
    val Parsed.Success(programTree, _) = parse(
      TestPrograms.lookup.withDirective("lookupScala", "lookup", "+list", "-elem", "-position", "-listNoElem"),
      ParsingRules.program(_)
    )
    programTree shouldEqual Program(
      Seq(TranslationDirective("lookupScala", "lookup", Seq(
        PredicateArgument("list", In),
        PredicateArgument("elem", Out),
        PredicateArgument("position", Out),
        PredicateArgument("listNoElem", Out)
      ))),
      Map(
        StructId("lookup", 4) -> Seq(
          Clause(
            Struct("lookup", Seq(
              ListTerm(Seq(Variable("H")), Some(Variable("T"))),
              Variable("H"),
              Struct("zero", Seq()),
              Variable("T")
            )),
            Seq()
          ),
          Clause(
            Struct("lookup", Seq(
              ListTerm(Seq(Variable("H")), Some(Variable("T"))),
              Variable("E"),
              Struct("s", Seq(Variable("N"))),
              ListTerm(Seq(Variable("H")), Some(Variable("T2")))
            )),
            Seq(
              Struct("lookup", Seq(
                Variable("T"),
                Variable("E"),
                Variable("N"),
                Variable("T2"))))))))
  }
  "A correct program, with invalid argument for translationDirective" should behave like correctProgramNonTranslatable(
    TestPrograms.lookup.withDirective("lookup", "lookup", "-list", "+elem", "+position", "-listNoElem")
  )
  "A correct program, with valid argument for translationDirective, but that requires an invalid translation of other predicates" should behave like correctProgramNonTranslatable (
    TestPrograms.permutation.withDirective("permutation", "permutation", "-list", "+permutations")
  )
}
