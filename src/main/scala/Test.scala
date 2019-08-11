import prolog2scala.lib.Clause._
import prolog2scala.lib.Predicate
import prolog2scala.lib.Cut._

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
  println(treeToString(
    FOR(
      IF(REF("x") INT_< LIT(10)),
      VALFROM("i") := LIT(0) INT_TO LIT(2),
      VAL("x") := REF("i")
    ) DO BLOCK(
      Predef_println APPLY LIT("Hello")
    )
  ))
}

object TryPattern extends App {
  case class Banana(arg: String)
  val h2 = 2
  println(for ((h :: `h2` :: h3 :: t,Banana(y)) <- List((List(1,2,3,4,5), Banana("asd")), (List(1,2,3), Banana("asd")), (List(1,4,2), Banana("asd")))) yield (h,h2,h3,t,y))
}