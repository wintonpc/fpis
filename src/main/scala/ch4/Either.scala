package ch4

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- eb
    } yield f(a, b)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def traverse[E,A,B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E,List[B]]](Right(Nil))((a, bs) => f(a).map2(bs)(_::_))

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(identity)
}