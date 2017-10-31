package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = List.empty[A]
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
//  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    import m._
    val zeroLaw = forAll(gen) { a =>
      op(zero, a) == a
      op(a, zero) == a
    }
    val assocLaw = forAll(Gen.listOfN(3, gen)) {
      case List(a, b, c) => op(a, op(b, c)) == op(op(a, b), c)
    }
    zeroLaw && assocLaw
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if(as.isEmpty)
      m.zero
    else if (as.length == 1)
      f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

//  val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

