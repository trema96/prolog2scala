package prolog2scala.parsing

import prolog2scala.parsing.Term.Struct

case class Clause(head: Struct, body: Seq[Term])
