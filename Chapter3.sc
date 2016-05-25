sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
//
//  // Exercise 3.1
//  val x = List(1, 2, 3, 4, 5) match {
//    case Cons(x, Cons(2, Cons(4, _))) => x // MatchError
//    case Nil => 42 // MatchError
//    case Cons(x, Cons(y, Cons(4, Cons(4, _)))) => x + y // MatchError
//    case Cons(h, t) => h + List.sum(t) // first match -> 15
//    case _ => 101 // matches -> 101
//  }
//  // the result of x will  be 15

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(_,t) => t
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => List()
    case Cons(_,t) => Cons(h, t)
  }
}