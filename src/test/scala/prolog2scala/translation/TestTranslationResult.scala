package prolog2scala.translation

import org.scalatest.{FlatSpec, Matchers}

import TranslationResult.TranslationSeq

class TestTranslationResult extends FlatSpec with Matchers {
  val succeded: TranslationResult[String] = TranslationResult.Success("a")
  val failed: TranslationResult[String] = TranslationResult.Failure("a")

  def divideBy2IfEven(v: Int): TranslationResult[Int] =
    if (v % 2 == 0)
      TranslationResult.Success(v / 2)
    else
      TranslationResult.Failure(s"$v is odd")

  "A translation result" should "be immutable" in {
    val before = succeded
    succeded map (_ => "b")
    succeded shouldBe before
  }

  "A succeded translation result" should "correctly map its values to another succeded translation result" in {
    succeded map (_ => "b") shouldEqual TranslationResult.Success("b")
  }

  "A succeded translation result" should "correctly flatMap its values to another succeded translation result if the mapping is successful" in {
    succeded flatMap (_ => TranslationResult.Success("b")) shouldEqual TranslationResult.Success("b")
  }

  "A succeded translation result" should "correctly flatMap its values to a failed translation result if the mapping is unsuccessful" in {
    succeded flatMap (_ => TranslationResult.Failure("b")) shouldEqual TranslationResult.Failure("b")
  }

  "A failed translation result" should "map to self" in {
    failed map (_ => "b") shouldBe failed
  }

  "A failed translation result" should "flatMap to self" in {
    failed map (_ => "b") shouldBe failed
  }

  "translateMany" should "return a successfull translation result containing all the translated values if each element translates correctly" in {
    (List(2, 4, 6) translateMany divideBy2IfEven map (_.toList)) shouldEqual TranslationResult.Success(List(1,2,3))
  }

  "translateMany" should "return the first unsuccessful result when any translation fails" in {
    List(1, 2, 3) translateMany divideBy2IfEven shouldEqual TranslationResult.Failure("1 is odd")
  }
}
