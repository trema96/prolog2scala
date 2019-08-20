package prolog2scala.translation

object TestPrograms {
  val lookup: String =
    """
      |lookup([H|T],H,zero,T).
      |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
    """.stripMargin
  val permutation: String =
    """
      |member2([X|Xs],X,Xs).
      |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
      |permutation([],[]).
      |permutation(Xs,[X|Ys]) :-
      | member2(Xs,X,Zs),
      | permutation(Zs, Ys).
    """.stripMargin
  val father: String =
    """
      |son(X,Y):-father(Y,X),male(X).
      |grandfather(X,Z):-father(X,Y),father(Y,Z).
      |father(abraham,isaac).
      |father(terach,nachor).
      |father(terach,abraham).
      |male(isaac).
    """.stripMargin
  val same: String =
    """
      |#same: same(+list1, +list2)
      |same([],[]).
      |same([X|Xs],[X|Ys]):- same(Xs,Ys).
    """.stripMargin

  val correctProgramsData = List(
    ProgramData(
      "lookup",
      lookup.withDirective("lookup", "lookup", "+list", "-elem", "+position", "-listNoElem"),
      """|object TranslatedProgram {
         |  trait Lookup_position
         |  case class S(arg0: Lookup_position) extends Lookup_position
         |  case object Zero extends Lookup_position
         |  def lookup[A1](list: List[A1], position: Lookup_position): Stream[(A1, List[A1])] = Predicate[(List[A1], Lookup_position), (A1, List[A1])](Fact({
         |    case (h :: t, Zero) => (h, t)
         |  }), Rule({
         |    case (h :: t, S(n)) => for ((e, t2) <- lookup(t, n))
         |      yield (e, List(h) ++ t2)
         |  }))(list, position)
         |}""".stripMargin
    ),
    ProgramData(
      "permutation",
      permutation.withDirective("permutation", "permutation", "+list", "-permutations"),
      """|object TranslatedProgram {
         |  def permutation[A1](list: List[A1]): Stream[List[A1]] = Predicate[List[A1], List[A1]](Fact({
         |    case Nil => List()
         |  }), Rule({
         |    case xs => for {
         |      (x, zs) <- member2_ioo(xs)
         |      ys <- permutation(zs)
         |    } yield List(x) ++ ys
         |  }))(list)
         |  private def member2_ioo[A1](arg0: List[A1]): Stream[(A1, List[A1])] = Predicate[List[A1], (A1, List[A1])](Fact({
         |    case x :: xs => (x, xs)
         |  }), Rule({
         |    case x :: xs => for ((e, ys) <- member2_ioo(xs))
         |      yield (e, List(x) ++ ys)
         |  }))(arg0)
         |}""".stripMargin
    ),
    ProgramData(
      "father",
      father
        .withDirective("sonOf", "son", "-personA", "+personB")
        .withDirective("grandfatherOf", "grandfather", "-personA", "+personB"),
      """|object TranslatedProgram {
         |  trait Father_2_arg0
         |  case object Abraham extends Father_2_arg0
         |  case object Terach extends Father_2_arg0
         |  case object Nachor extends Father_2_arg0
         |  case object Isaac extends Father_2_arg0
         |  private def father_io(arg0: Father_2_arg0): Stream[Father_2_arg0] = Predicate[Father_2_arg0, Father_2_arg0](Fact({
         |    case Abraham => Isaac
         |  }), Fact({
         |    case Terach => Nachor
         |  }), Fact({
         |    case Terach => Abraham
         |  }))(arg0)
         |  private def father_ii(arg0: Father_2_arg0, arg1: Father_2_arg0): Stream[Unit] = Predicate[(Father_2_arg0, Father_2_arg0), Unit](Fact({
         |    case (Abraham, Isaac) => ()
         |  }), Fact({
         |    case (Terach, Nachor) => ()
         |  }), Fact({
         |    case (Terach, Abraham) => ()
         |  }))(arg0, arg1)
         |  def grandfatherOf(personB: Father_2_arg0): Stream[Father_2_arg0] = Predicate[Father_2_arg0, Father_2_arg0](Rule({
         |    case z => for {
         |      (x, y) <- father_oo()
         |      () <- father_ii(y, z)
         |    } yield x
         |  }))(personB)
         |  private def male_i(arg0: Father_2_arg0): Stream[Unit] = Predicate[Father_2_arg0, Unit](Fact({
         |    case Isaac => ()
         |  }))(arg0)
         |  def sonOf(personB: Father_2_arg0): Stream[Father_2_arg0] = Predicate[Father_2_arg0, Father_2_arg0](Rule({
         |    case y => for {
         |      x <- father_io(y)
         |      () <- male_i(x)
         |    } yield x
         |  }))(personB)
         |  private def father_oo(): Stream[(Father_2_arg0, Father_2_arg0)] = Predicate[Unit, (Father_2_arg0, Father_2_arg0)](Fact({
         |    case _ => (Abraham, Isaac)
         |  }), Fact({
         |    case _ => (Terach, Nachor)
         |  }), Fact({
         |    case _ => (Terach, Abraham)
         |  }))()
         |}""".stripMargin
    ),
  )

  case class ProgramData(programName: String, program: String, expectedTranslation: String)

  implicit class ProgramString(base: String) {
    def withDirective(scalaName: String, predName: String, args: String*): String =
      s"#$scalaName: $predName(${args.mkString(", ")})\n" + base
  }
}
