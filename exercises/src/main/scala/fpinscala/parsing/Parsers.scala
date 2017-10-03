package fpinscala.parsing

import fpinscala.testing._

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.head)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    val p1 = if (n == 1) succeed(List.empty) else listOfN(n - 1, p)
    (p map2 p1) (_ :: _)
  }

  def wrap[A](p: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    (p map2 wrap(many(p))) (_ :: _) or succeed(List.empty)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a flatMap (av => succeed(f(av)))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    (p map2 p.many) (_ :: _)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    a ** b map f.tupled

  def map2ViaFlatMap[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def regex[A](r: Regex): Parser[A]

  val numCharParser = flatMap(regex("\\d+".r))(listOfN(_, char('a')))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def many1: Parser[List[A]] = self.many1(p)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    def succeedLaw[A](a: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(a)(s) == Right(a))

    def productLaw[A, B](a: Parser[A], b: Parser[B])(in: Gen[String]): Prop = {
      Prop.forAll(in)(s => run(a ** b)(s) == Right((a, b)))
    }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}