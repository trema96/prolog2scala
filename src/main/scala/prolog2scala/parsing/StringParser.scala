package prolog2scala.parsing

import fastparse._
import PrologWhitespace._

object StringParser {
  def lowercaseLetter[_: P]: P[String] = CharIn("a-z").!
  def uppercaseLetter[_: P]: P[String] = CharIn("A-Z").!
  def digit[_: P]: P[String] = CharIn("0-9").!
  def nameCharacter[_: P]: P[String] = P(lowercaseLetter | uppercaseLetter | digit | "_".!)
  def name[_: P]: P[String] = P(
    P(lowercaseLetter ~~ nameCharacter.repX).! |
    ("'" ~ CharsWhile(_ != '\'').! ~ "'") |
    ("\"" ~ CharsWhile(_ != '"').! ~ "\"")
  )
  def variableName[_: P]: P[String] = P((uppercaseLetter | "_".!) ~~ nameCharacter.repX).!
  def number[_: P]: P[String] = digit.rep(1).!
  def intString[_: P]: P[String] = P("-".?.! ~ number).!
  def floatString[_: P]: P[String] = P("-".?.! ~ (number ~ ".".! ~ number).!).!
}
