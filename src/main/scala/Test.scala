import fastparse.Parsed
import prolog2scala.lib.Clause._
import prolog2scala.lib.Predicate
import prolog2scala.lib.Cut._
import prolog2scala.translation.TranslationResult
import prolog2scala.translation.parsing.ParsingRules

object Test extends App {
  def lookup[A](list: List[A], elem: A): Stream[Int] = Predicate[(List[A], A), Int](
    Fact{case (e1::_,e2) if e1==e2 => 0},
    Rule{case (_ :: t, e) => for (n <- lookup(t, e))  yield n+1}
  )((list, elem))

  //println(lookup(List(1,2,3,4,1,2,3,4), 2) toList)

  //x(X):- (a(X), !; b(X)).
  //x(X):- c(X).
  //a(1).
  //a(2).
  //b(3).
  //b(4).
  //c(5).
  //c(6).
}

object Asd extends App {
  /*
  def lister[A](el: A): Stream[A] = Stream(el).map(e => {println(e);e})

  val list = lister(0) #::: lister(1) #::: lister(2) #::: lister(3) #::: lister(2) #::: lister(4)

  import prolog2scala.common.Implicits._
  println(list.iterator.takeUntil(_ == 2) toList)
  println(list.iterator.takeUntil(_ == 5) toList)
  println("NoCut")
  println((for (x <- lister(1) #::: lister(2) #::: lister(3);
        y <- lister(4) #::: lister(5) #::: lister(6)) yield x + "_" + y) toList)

  println("WithCut")
  val res = (for (x <- lister(1) #::: lister(2) #::: lister(3)) yield x) !
    (x => for (y <- lister(4) #::: lister(5) #::: lister(6)) yield x + "_" + y)
  println(res.didCut)
  println(res.solutions toList)

  println("WithCutNoSucc")
  val res2 = (for (x <- Stream[Int]()) yield x) !
    (x => for (y <- lister(4) #::: lister(5) #::: lister(6)) yield x + "_" + y)
  println(res.didCut)
  println(res.solutions toList)

  def noL = Stream[String]()
  def l1 = lister("a1") #::: lister("a2") #::: lister("a3")
  def l2 = lister("b1") #::: lister("b2") #::: lister("b3")
  def l3 = lister("c1") #::: lister("c2") #::: lister("c3")

  var res: CutResult[String] = _

  res = ! l1
  println(res.didCut)
  println(res.solutions.toList)

  res = l1.!
  println(res.didCut)
  println(res.solutions toList)

  res = noL ! (_ => l1)
  println(res.didCut)
  println(res.solutions toList)

  res = ! noL
  println(res.didCut)
  println(res.solutions toList)

  res = ! l1 ! (x => l2.map(x + "_" + _)) ! (x => l3.map(x + "_" + _))
  println(res.didCut)
  println(res.solutions toList)
  */
}

object ProvaFor extends App {
  println(for (a <- List(1,2,3);
               b <- List("a", "b") if a < 3) yield (a,b))

  //implicit def intToList(x: Int) = List(x)
  //implicit def doubleToList(x: Double) = List(x)

  println(for (x <- List(1,2,3);
               x1 = x + 1) yield (x, x1))
}

import treehugger.forest._
import definitions._
import treehuggerDSL._

object TryTree extends App {
  val params = List("a","b")
  implicit def paramToValDef(param: ValNameStart): ValDef = param.mkTree(EmptyTree)
  implicit def paramSeqToValDef(param: Seq[ValNameStart]): Seq[ValDef] = param map paramToValDef
  println(treeToString(
    DEF("x", IntClass) withParams (params map (PARAM(_, IntClass))) := REF("y")
  ))
}

import fastparse._
object TryTranslation extends App {
  val program1 =
    """
      |#lookup: lookup(+list, -elem, +position, -listNoElem)
      |lookup([H|T],H,zero,T).
      |lookup([H|T],E,s(N),[H|T2]):- lookup(T,E,N,T2).
    """.stripMargin
  val program2 =
    """
      |#permutation: permutation(+list, -permutations)
      |member2([X|Xs],X,Xs).
      |member2([X|Xs],E,[X|Ys]):-member2(Xs,E,Ys).
      |permutation([],[]).
      |permutation(Xs,[X|Ys]) :-
      | member2(Xs,X,Zs),
      | permutation(Zs, Ys).
    """.stripMargin
  val program3 =
    """
      |#sonOf: son(-personA, +personB)
      |#grandfatherOf: grandfather(-personA, +personB)
      |son(X,Y):-father(Y,X),male(X).
      |grandfather(X,Z):-father(X,Y),father(Y,Z).
      |father(abraham,isaac).
      |father(terach,nachor).
      |father(terach,abraham).
      |male(isaac).
    """.stripMargin
  val program4 =
    """
      |#same: same(+list1, +list2)
      |same([],[]).
      |same([X|Xs],[X|Ys]):- same(Xs,Ys).
    """.stripMargin

  val program = program4
  val Parsed.Success(parseResult, _) = parse(program, ParsingRules.program(_))
  val TranslationResult.Success(tree) = parseResult.translate()
  println(treeToString(tree))
  /*
  val TranslationResult.Success(types) = parseResult.typeCheck()
  println(types._1)
  types._2.foreach(x => println(treeToString(x)))
  */
}

object TryTranslated extends App {
  object TranslatedProgram {
    def same[A1](list1: List[A1], list2: List[A1]): Stream[Unit] = Predicate[(List[A1], List[A1]), Unit](Fact({
      case (Nil, Nil) => ()
    }), Rule({
      case (x :: xs, x :: ys) => for (() <- same(xs, ys))
        yield ()
    }))(list1, list2)
  }
  import TranslatedProgram._
  //println(lookup(List(1,2,3,4), S(Zero)) toList)
  //println(permutation(List(1,2,3,4)) toList)
  //println(sonOf(Abraham) toList)
  //println(grandfatherOf(Isaac) toList)
  println(same(List("a","b","c"),List(1,2,3)) toList)
  println(same(List("a",2,3),List("a",2,3)) toList)
}

object TryPattern extends App {
  case class Banana(arg: String)
  val h2 = 2
  println(for ((h :: `h2` :: h3 :: t,Banana(y)) <- List((List(1,2,3,4,5), Banana("asd")), (List(1,2,3), Banana("asd")), (List(1,4,2), Banana("asd")))) yield (h,h2,h3,t,y))
}