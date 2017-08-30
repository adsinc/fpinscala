package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt
    if(n >= 0) (n, newRng)
    else if(n == Int.MinValue) (Int.MaxValue, newRng)
    else (-1, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, newRng) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, newRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, g1) = rng.nextInt
    val (d, g2) = double(g1)
    ((i, d), g2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), g) = intDouble(rng)
    ((d, i), g)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, xs: List[Int], r: RNG): (List[Int], RNG) =
      if(count == 0) (xs, r)
      else {
        val (i, r2) = r.nextInt
        loop(count - 1, i :: xs, r2)
      }

    loop(count, Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
