object Ch3 {
  def setHead[A](newHead: A, xs: List[A]): List[A] = newHead :: xs.tail

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n == 0) xs
    else drop(xs.tail, n-1)
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = {
    if (f(xs.head)) dropWhile(xs.tail, f)
    else xs
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case x :: Nil => Nil
    case x :: rest => x :: init(rest)
  }

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case x :: xs => f(x, foldRight(xs, acc)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, length) => length + 1)

  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case x :: xs => foldLeft(xs, f(acc, x))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((xs, x) => x :: xs)

  def foldLeft2[A, B](as: List[A], acc: B)(f: (B, A) => B) =
    foldRight(reverse(as), acc)((a, b) => f(b, a))

  def foldRight2[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), acc)((b, a) => f(a, b))

  def append[A](a: List[A], b: List[A]): List[A] = foldRight2(a, b)(::(_, _))

  def concat[A](xss: List[List[A]]) = foldRight2(xss, Nil:List[A])(append)

  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    foldRight2(xs, Nil:List[B])((a, bs) => f(a) :: bs)
  }

  def odd(x: Int): Boolean = x % 2 == 1

  def filter[A](as: List[A])(pred: A => Boolean) = {
    foldRight2(as, Nil:List[A])((a, as) => if (pred(a)) a :: as else as)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val reversedBs = foldLeft(as, Nil:List[List[B]])((bs, a) => f(a) :: bs)
    foldLeft(reversedBs, Nil:List[B])((x, y) => append(y, x))
  }

  def flatMapSlow[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + ((t1 - t0) / 1000000000.0) + "s")
    result
  }

  def filter2[A](as: List[A])(pred: A => Boolean) = {
    flatMap(as)(a => if (pred(a)) List(a) else Nil)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (a :: as, b :: bs) => f(a, b) :: zipWith(as, bs)(f)
    case _ => Nil
  }

  def startsWith[A](as: List[A], bs: List[A]): Boolean = (as, bs) match {
    case (_, Nil) => true
    case (a :: as, b :: bs) if a == b => startsWith(as, bs)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup.startsWith(sub) || (!sup.isEmpty && hasSubsequence(sup.tail, sub))

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(mapLeaf: A => B)(combine: (B, B) => B): B = t match {
    case Leaf(v) => mapLeaf(v)
    case Branch(l, r) => combine(fold(l)(mapLeaf)(combine), fold(r)(mapLeaf)(combine))
  }

  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => leaf(f(v)))(Branch(_, _))

  def sizeWithFold[A](t: Tree[A]): Int =
    fold(t)(v => 1)(1 + _ + _)

  def maximumWithFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthWithFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def leaf[A](v: A): Tree[A] = Leaf(v)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]