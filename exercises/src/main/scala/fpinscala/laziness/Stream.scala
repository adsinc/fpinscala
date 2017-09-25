package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List.empty[A])((a, acc) => a :: acc)

  def take(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if(f(a)) cons(a, b)
      else b
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => b append f(a))

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((ah() -> bh(), at() -> bt()))
      case _ => None
    }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
      case _ => None
    }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, bs)) {
      case (Empty, Cons(bh, bt)) => Some(
        (None -> Option(bh())) ->
          (empty -> bt())
      )
      case (Cons(ah, at), Empty) => Some(
        (Option(ah()) -> None) ->
          (at() -> empty)
      )
      case (Cons(ah, at), Cons(bh, bt)) => Some(
        (Option(ah()) -> Option(bh())) -> (at() -> bt())
      )
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile(_._2.isDefined)
      .forAll {case (a, b) => a == b}

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(_, t) => Some(t() -> t())
    case _ => None
  } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val b = f(a, acc._1)
      b -> cons(b, acc._2)
    })._2
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
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n))

  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s + 1, s + 1))

  def fibs: Stream[Int] = unfold((1, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}