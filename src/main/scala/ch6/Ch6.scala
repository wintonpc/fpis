package ch6

object Ch6 {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    val result = n match {
      case Int.MinValue => 0
      case n if n < 0 => -n
      case _ => n
    }
    (result, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    val result = i.toDouble / Int.MaxValue.toDouble
    (result, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (a, x) = rng.nextInt
    val (b, y) = double(x)
    ((a, b), y)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (a, x) = double(rng)
    val (b, y) = x.nextInt
    ((a, b), y)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (a, x) = double(rng)
    val (b, y) = double(x)
    val (c, z) = double(y)
    ((a,b,c), z)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def p(count: Int, xs: List[Int], rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (n, next) = rng.nextInt
        p(count - 1, n :: xs, next)
      } else {
        (xs, rng)
      }
    }
    p(count, Nil, rng)
  }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  val doubleViaMap =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    x => {
      val (a, y) = ra(x)
      val (b, z) = rb(y)
      (f(a, b), z)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val intDoubleViaMap2 = both(_.nextInt, double)

  val doubleIntViaMap2 = both(double, _.nextInt)

  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] =
    xs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(_.nextInt))


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    s1 => {
      val (a, s2) = f(s1)
      val (b, s3) = g(a)(s2)
      (b, s3)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
   flatMap(nonNegativeInt) { i =>
     val mod = i % n
     if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
   }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }
}
