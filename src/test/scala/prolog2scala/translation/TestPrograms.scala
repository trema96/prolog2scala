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
      List(
        ("lookup(List(1,2,3,4), S(S(Zero))) toList", "List((3, List(1,2,4)))"),
        ("lookup(List(1,2,3,4), Zero) toList", "List((1, List(2,3,4)))")
      )
    ),
    ProgramData(
      "permutation",
      permutation.withDirective("permutation", "permutation", "+list", "-permutations"),
      List(
        ("permutation(List(1,2,3)) toSet", "Set(List(1,2,3), List(1,3,2), List(2,1,3), List(2,3,1), List(3,1,2), List(3,2,1))"),
        ("permutation(List()) toList", "List(List())")
      )
    ),
    ProgramData(
      "father",
      father
        .withDirective("sonOf", "son", "-personA", "+personB")
        .withDirective("grandfatherOf", "grandfather", "-personA", "+personB"),
      List(
        ("grandfatherOf(Isaac) toList", "List(Terach)"),
        ("sonOf(Abraham) toList", "List(Isaac)")
      )
    )
  )

  case class ProgramData(programName: String, program: String, tests: Seq[(String, String)])

  implicit class ProgramString(base: String) {
    def withDirective(scalaName: String, predName: String, args: String*): String =
      s"#$scalaName: $predName(${args.mkString(", ")})\n" + base
  }
}
