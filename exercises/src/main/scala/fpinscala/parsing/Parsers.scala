package fpinscala.parsing

import java.util.regex.Pattern

import fpinscala.testing._

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
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

  implicit def regex[A](r: Regex): Parser[String]

  def skipL[A](l: Parser[Any], r: Parser[A]): Parser[A] =
    map2(l, r)((_, r) => r)

  def skipR[A](l: Parser[A], r: Parser[Any]): Parser[A] =
    map2(l, r)((l, _) => l)

  def between[A](l: Parser[Any], r: Parser[Any])(p: => Parser[A]): Parser[A] =
    l *> p <* r

  def split1[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
    map2(p, many(sep *> p))(_ :: _)

  def split[A, B](a: Parser[A], sep: Parser[B]): Parser[List[A]] =
    split1(a, sep) or succeed(List())

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  private def quoted: Parser[String] = "\"" *> thru("\"") map (_.dropRight(1))

  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  def double: Parser[Double] =
    "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r map (_.toDouble) label "double literal"

  def whiteSpace: Parser[String] = "\\s+".r

  def token[A](p: Parser[A]): Parser[A] = p <* whiteSpace

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

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
    def *>[B](p2: Parser[B]): Parser[B] = self.skipL(p, p2)
    def <*(p2: Parser[Any]): Parser[A] = self.skipR(p, p2)
    def split(sep: String): Parser[List[A]] = self.split(p, sep)
    def token: Parser[A] = self.token(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def label(msg: String): Parser[A] = self.label(msg)(p)
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

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
  }
}

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    def array: Parser[JArray] = between("[", "]")(
      value split "," map (_.toIndexedSeq) map JArray scope "array"
    )
    def obj: Parser[JObject] = between("{", "}")(
      entry split "," map (_.toMap) map JObject scope "object"
    )

    def entry: Parser[(String, JSON)] = escapedQuoted ** (":" *> value)
    def bool: Parser[JBool] = "true" | "false" map (_.toBoolean) map JBool

    def literal: Parser[JSON] = scope("literal")(
      "null".token.map(_ => JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        bool
    )
    def value: Parser[JSON] = literal | obj | array
    whiteSpace *> (obj | array)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

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

case class MyParser[+A](parse: String => Either[ParseError, A])

object MyParsers extends Parsers[MyParser] {

  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = ???

  override def or[A](s1: MyParser[A], s2: => MyParser[A]): MyParser[A] = ???

  implicit def string(s: String): MyParser[String] = MyParser { input =>
    if (s == input) Right(s)
    else Left(ParseError(
      stack = List(Location(input) -> s"Expected $s, actual $input")
    ))
  }

  override implicit def regex[A](r: Regex): MyParser[String] = MyParser {
    case input@r() => Right(input)
    case input => Left(ParseError(
      stack = List(Location(input) -> s"Unknown token $input")
    ))
  }

  override def slice[A](p: MyParser[A]): MyParser[String] = MyParser {
    input =>
      p parse input match {
        case Right(_) => Right(input)
        case Left(error) => Left(error)
      }
  }

  override def wrap[A](p: => MyParser[A]): MyParser[A] = ???

  override def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???

  override def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???

  override def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???

  override def attempt[A](p: MyParser[A]): MyParser[A] = ???
}