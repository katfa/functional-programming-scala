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

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }
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
    case Nil => Nil
    case Cons(_,t) => t
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,t) => Cons(h, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => f(h) match {
      case true => dropWhile(t, f)
      case false => l
    }
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, counter) => counter + 1)
  }

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Exercise 3.11
  def sumLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0)((acc, n) => acc + n)
  }

  def productLeft(ns: List[Double]): Double = {
    foldLeft(ns, 1.0)((acc, n) => acc * n)
  }

  def lengthLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((counter,_) => counter + 1)
  }
}

val l = List(1,2,3,4,5)
val doubles = List(1.0,2.0,3.0)
println(List.sumLeft(l))
println(List.productLeft(doubles))
println(List.lengthLeft(doubles))
