object Chapter2 {
  // Exercise 2.1 - Get the nth Fibonacci number
  def fib(n: Int): Int = {
    def go(counter: Int, prev: Int, acc: Int): Int = {
      if (counter == n) acc
      else go(counter + 1, acc, acc + prev)
    }

    go(0, 1, 0)

  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def iter(i: Int): Boolean = {
      if (as.length >= 1) true
      else !ordered(as(i + 1), as(i)) && iter(i + 1)
    }
    iter(0)
  }

  // Exercise 2.3 - currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def cur(a: A): B => C = {
      (b: B) => {
        f(a, b)
      }
    }

    cur
  }

  // Exercise 2.5 HO function that composes two functions
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => {
      f(g(a))
    }
  }

}



