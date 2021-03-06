package fpinscala.testing

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.laziness._
import fpinscala.parallelism.Par.Par
import fpinscala.parallelism._
import fpinscala.state._
import fpinscala.testing.Gen._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    case (max, n, rng) => run(max, n, rng) match {
      case Passed|Proved => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    case (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace.mkString("\n")}
     """.stripMargin

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop{ (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool()
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get() == Par.unit(2)(ES).get()
  )

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val p2 = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get() == p2(ES).get()
  }

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p1, p2)(_ == _)

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get()
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool()) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) {case (s, a) => f(a)(s).get() }

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) {case s ** a => f(a)(s).get() }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p3_1 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0, 10) map Par.unit
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val p5 = forAllPar(pint)(n => equal(Par.fork(n), n) )
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s, this))

  def unsized: SGen[A] = SGen[A] (_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(n => n % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2
    val threshold = w1 / (w1 + w2)
    Gen(State(RNG.double)) flatMap (w => if(w <= threshold) gen1 else gen2)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => g.listOfN(n)
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(Gen.listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => g.listOfN(n max 1)
  }
  val maxProp1 = forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = forAll(Gen.listOf(smallInt)) { xs =>
    val sorted = xs.sorted
    sorted.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists {case (a, b) => a > b }
  }

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = Gen(
    State {rng =>
      val (seed, rng2) = rng.nextInt
      val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
      (f, rng2)
    }
  )

  val takeDropProp = {
    val gen = SGen(s => smallInt.map2(Gen.listOf(smallInt)(s))(_ -> _))
    forAll(gen) { case (n, xs) =>
      val taken = xs.take(n)
      val dropped = xs.drop(n)
      taken.length == ((n max 0) min xs.length) &&
        dropped.length == ((xs.length - (n max 0)) max 0) &&
        taken ++ dropped == xs
    }
  }

  val opSeqProp = {
    import fpinscala.errorhandling._
    forAll(Gen.listOf(smallInt)) { xs =>
      val optList: List[Option[Int]] = xs.map(Some(_))
      Option.sequence(optList) == Some[List[Int]](xs)
    }
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)
}

case class SGen[+A](g: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen {
    g(_).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    n => g(n).flatMap(a => f(a).g(n))
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}