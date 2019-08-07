package prolog2scala.parsing

sealed trait Term

object Term {
  case class Struct(name: String, args: Term*) extends Term {
    override def toString: String = if (args isEmpty) name else name + "(" + args.mkString(",") + ")"
  }

  case class Variable(name: String) extends Term

  sealed trait NumberTerm extends Term
  object NumberTerm {
    case class Int(value: scala.Int) extends NumberTerm
    case class Float(value: Double) extends NumberTerm
  }

  case object Cut extends Term

  case class ListTerm(values: Seq[Term], tail: Option[Term]) extends Term {
    override def toString: String = "[" + values.mkString(",") + tail.map("|" + _).getOrElse("") + "]"
  }
}
