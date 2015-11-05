package ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if (f(v)) this else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C) = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case a :: as => map2(a, sequence(as))(_::_)
  }

  def sequenceViaFold[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, as) => map2(a, as)(_::_))

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, bs) => map2(f(a), bs)(_::_))

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}