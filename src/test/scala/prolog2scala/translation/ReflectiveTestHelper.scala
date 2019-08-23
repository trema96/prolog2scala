package prolog2scala.translation

object ReflectiveTestHelper {
  /**
    * Used on reflection
    * @param tests (what is testing, what the test gave, what is expected
    * @return a string containing all errors details if there were any or else None
    */
  def test(tests: (String, Any, Any)*): Option[String] = {
    val failedTestsMsgs = tests.flatMap{case (what, actual, expected) =>
      if (actual == expected)
        None
      else
        Some(
          s"""
             |Failed test: $what
             |Actual: $actual
             |Expected: $expected
           """.stripMargin)
    }
    if (failedTestsMsgs isEmpty)
      None
    else
      Some(failedTestsMsgs.mkString("\n"))
  }

  def prepareCode(scalaCode: String, tests: Seq[(String, String)]): String =
    imports + "\n" + scalaCode + "\n"  + makeTests(tests)

  private val imports: String =
    """
      |import prolog2scala.lib._
      |import prolog2scala.lib.Cut._
      |import prolog2scala.lib.Clause._
      |import prolog2scala.translation.ReflectiveTestHelper._
      |import TranslatedProgram._
    """.stripMargin

  private def makeTests(tests: Seq[(String, String)]): String =
    "test(" + tests.map(test => "(\"" + test._1 + "\"," + test._1 + "," + test._2 + ")").mkString(",\n") + ")"
}
