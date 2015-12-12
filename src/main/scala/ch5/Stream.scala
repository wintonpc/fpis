package ch5

import ch5.Stream._

import scala.Option
import scala.Function.tupled

sealed trait Stream[+A] {
  //  def iterator: Iterator[A] = {
  //    var stream = this
  //    new Iterator[A] {
  //      def hasNext: Boolean = stream != Empty
  //
  //      def next: A = stream match {
  //        case Cons(h, t) => {
  //          stream = t()
  //          h()
  //        }
  //      }
  //    }
  //  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(dh, dt) => dh() :: dt().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(dh, dt) => if (n == 0) Empty else cons(dh(), dt().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_, dt) => if (n == 0) this else dt().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(dh, dt) => if (p(dh())) cons(dh(), dt().takeWhile(p)) else Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case Empty => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])

  def headOption: Option[A] =
    foldRight(None:Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Ch5.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Ch5.unfold((this, n)) {
      case (Empty, _) => None
      case (_, 0) => None
      case (Cons(h, t), n) => Some((h(), (t(), n-1)))
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Ch5.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    Ch5.unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _ => None
    }

  def zipAll[B](bs: Stream[B]) : Stream[(Option[A], Option[B])] =
    Ch5.unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
      case (Cons(a, as), Empty) => Some((Some(a()), None), (as(), empty))
      case (Empty, Cons(b, bs)) => Some((None, Some(b())), (empty, bs()))
      case _ => None
    }

  def startsWith[A](bs: Stream[A]): Boolean =
    this.zipAll(bs).foldRight(true)((t, z) => t match {
      case (Some(a), Some(b)) if a == b => z
      case (Some(a), None) => true
      case _ => false
    })

  def startsWith2[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.nonEmpty).forAll(tupled(_ == _))

  def tails: Stream[Stream[A]] =
    Ch5.unfold(this)(s => s match {
      case Empty => None
      case Cons(_, t) => Some((s, t()))
    })

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight(cons(z, empty))((a, b) => {
      lazy val b2 = b
      b2 match { case Cons(h, _) => cons(f(a, h()), b) }
    })

  def nth(n: Int): A = drop(n-1).take(1).toList.head
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Ch5 {
  def naturals(x: Int = 1): Stream[Int] = cons(x, naturals(x + 1))
  def naturals: Stream[Int] = naturals(1)

  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, fibs(b, a + b))

  def factorials: Stream[Int] =
    unfold((1, 1))({case (n, f) => Some((f * n, (n + 1, f * n)))})

  def factorial(n: Int): Int = factorials.nth(n)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def fibsViaUnfold: Stream[Int] = unfold((0, 1))({case (a, b) => Some((a, (b, a + b)))})
}