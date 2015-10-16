object Ch2 {

  def fib(n: Int): Int = {
    if (n < 2) n
    else fib(n-1) + fib(n-2)
  }

  def isSorted[A](xs: List[A], ordered: (A, A) => Boolean): Boolean = xs match {
    case Nil => true
    case List(x) => true
    case x :: y :: rest => ordered(x, y) && isSorted(y :: rest, ordered)
  }

  def isSorted[A](xs: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= xs.length) true
      else ordered(xs(n-1), xs(n)) && loop(n+1)
    }

    if (xs.length < 2) true
    else loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}