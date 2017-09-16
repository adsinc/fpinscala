import fpinscala.errorhandling._
import fpinscala.errorhandling.Either._

def parseInsuranceRateQuote(age: String,
                            numberOfSpeedingTickets: String): Either[Exception, Double] =
  for {
    a <- Try(age.toInt)
    tickets <- Try(numberOfSpeedingTickets.toInt)
  } yield a * tickets

parseInsuranceRateQuote("1w0", "21")

traverse(List("1", "2"))(x => Try(x))

sequence(List(Try("10".toInt), Try("20".toInt)))