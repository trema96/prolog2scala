package prolog2scala.translation

import prolog2scala.translation.Term.Variable

sealed trait Term {
  def variables: Set[Variable]
}

object Term {
  case class Struct(name: String, args: Seq[Term]) extends Term {
    override def variables: Set[Variable] = args flatMap (_.variables) toSet

    override def toString: String = if (args isEmpty) name else name + "(" + args.mkString(",") + ")"
  }

  case class Variable(name: String) extends Term {
    override def variables: Set[Variable] = Set(this)
  }

  sealed trait NumberTerm extends Term {
    override def variables: Set[Variable] = Set.empty
  }

  object NumberTerm {
    case class Int(value: scala.Int) extends NumberTerm
    case class Float(value: Double) extends NumberTerm
  }

  case object Cut extends Term {
    override def variables: Set[Variable] = Set.empty
  }

  case class ListTerm(values: Seq[Term], tail: Option[Term]) extends Term {
    override def variables: Set[Variable] = (values ++ tail) flatMap (_.variables) toSet

    override def toString: String = "[" + values.mkString(",") + tail.map("|" + _).getOrElse("") + "]"
  }
}
