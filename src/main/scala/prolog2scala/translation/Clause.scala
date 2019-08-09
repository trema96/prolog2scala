package prolog2scala.translation

import prolog2scala.translation.Term.Struct

case class Clause(head: Struct, body: Seq[Term])
