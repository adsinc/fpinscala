package fpinscala.monoids

import fpinscala.parallelism.Nonblocking.Par.toParOps
import fpinscala.parallelism.Nonblocking._

import scala.language.higherKinds

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

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type Acc = (Int, Boolean)
    val m = new Monoid[Acc] {
      def op(a1: Acc, a2: Acc): Acc = {
        val (prevInt, isOrd) = a1
        val (currInt, _) = a2
        currInt -> (prevInt <= currInt && isOrd)
      }

      val zero: Acc = ints.head -> true
    }
    foldMapV(ints, m)(Tuple2.apply(_, true))._2
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = (a1 map2 a2) (m.op)
    val zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(l, c, r)) => Part(s1 + l, c, r)
      case (Part(l, c, r), Stub(s2)) => Part(l, c, r + s2)
      case (Part(l1, c1, r1), Part(l2, c2, r2)) =>
        Part(l1, c1 + c2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }

    val zero: WC = Stub("")
  }

  def count(s: String): Int = {

    def wc(c: Char) =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(str) => unstub(str)
      case Part(lStub, count, rStub) => unstub(lStub) + count + unstub(rStub)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = A.op(a1._1, a2._1) -> B.op(a1._2, a2._2)

    def zero: (A, B) = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(a1: A => B, a2: A => B): A => B =
        a => B.op(a1(a), a2(a))

      val zero: A => B =
        _ => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map()

      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(
            a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)
          ))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  override def toList[A](as: List[A]) = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(value) => f(z, value)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(value) => f(value, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
}

