package fpinscala.state

import fpinscala.state.RNG.Rand
import fpinscala.state.State.{get, modify}


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

//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, newRng) = rng.nextInt
    if(n >= 0) (n, newRng)
    else if(n == Int.MinValue) (Int.MaxValue, newRng)
    else (-n, newRng)
  }

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)(rng)

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

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill[Rand[Int]](count)(_.nextInt))(rng)

//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, ar) = ra(rng)
//      val (b, br) = rb(ar)
//      (f(a, b), br)
//    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight(List.empty[A], rng)((f, acc) => {
        val (as, r) = acc
        val (a, r1) = f(r)
        (a :: as, r1)
      })
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r2) = f(rng)
      g(a)(r2)
    }

  def nonNegativeLessThat(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if(i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThat(n)
    })
}

case class State[S,+A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb map (b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, r2) = run(s)
      f(a).run(r2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map(modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)

  def update(i: Input)(m: Machine): Machine = (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Coin, Machine(true, candies, coins)) =>
        Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        Machine(locked = true, candies - 1, coins)
    }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) => f.map2(acc)(_ :: _))
}
