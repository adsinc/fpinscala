package fpinscala.parallelism

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent._

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      new Future[C] {
        def cancel(mayInterruptIfRunning: Boolean): Boolean = true

        def isCancelled: Boolean = af.isCancelled || bf.isCancelled

        def isDone: Boolean = af.isDone && bf.isDone

        def get(): C = f(af.get, bf.get)

        def get(timeout: Long, unit: TimeUnit): C = {
          val startTime = System.currentTimeMillis()

          val a = af.get(timeout, unit)
          val runTime = System.currentTimeMillis() - startTime
          val b = bf.get(unit.toMillis(timeout) - runTime, MILLISECONDS)
          f(a, b)
        }
      }
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val choice = run(es)(n).get
      choices(choice)(es)
    }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = fork {
    ps.foldRight(unit(List.empty[A]))((pa, acc) => map2(pa, acc)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A, B](ps: List[A])(p: A => Boolean): Par[List[A]] = {
    val pars = ps map asyncF(Some(_).filter(p))
    map(sequence(pars))(_.flatten)
  }

  def parSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      map2(fork(parSum(l)), fork(parSum(r)))(_ + _)
    }

  def fold[A](as: IndexedSeq[A])(z: A)(f: (A, A) => A) : Par[A] = fork {
    if(as.size <= 1)
      unit(as.headOption getOrElse z)
    else {
      val(l, r) = as.splitAt(as.length / 2)
      map2(fork(fold(l)(z)(f)), fold(r)(z)(f))(f)
    }
  }

  def countWords(texts: List[String]): Par[Int] = fork {
    val d = parMap(texts)(_.split("\\s+").length)
    map(d)(counts => counts.sum)
  }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    val fp = map2(a, b)((aa, bb) => f.curried(aa)(bb))
    map2(fp, c)((fpar, cc) => fpar(cc))
  }

  def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] = {
    val fp = map3(a, b, c)((aa, bb, cc) => f.curried(aa)(bb)(cc))
    map2(fp, d)((fpar, dd) => fpar(dd))
  }

  def map5[A,B,C,D,E,F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] = {
    val fp = map4(a, b, c, d)((aa, bb, cc, dd) => f.curried(aa)(bb)(cc)(dd))
    map2(fp, e)((fpar, ee) => fpar(ee))
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
