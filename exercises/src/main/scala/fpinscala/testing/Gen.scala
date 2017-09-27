package fpinscala.testing

import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.laziness._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    case (n, rng) => run(n, rng) match {
      case Passed => p.run(n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    case (n, rng) => run(n, rng) match {
      case Falsified(msg, _) => p.tag(msg).run(n, rng)
      case x => x
    }
  }

  def tag(msg: String) = Prop {
    (n,rng) => run(n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
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
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(x => f(x).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s, this))

  def unsized: SGen[A] = SGen[A] (_ => this)
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
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen {
    n => forSize(n).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    n => forSize(n).flatMap(a => f(a).forSize(n))
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => Gen.listOfN(n, g)
  }
}