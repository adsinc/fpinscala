package fpinscala.streamingio

import java.io.File
import java.util.concurrent.Executors

import fpinscala.iomonad._
import fpinscala.streamingio.SimpleStreamTransducers.Process._

object ConvertFToC extends App {
  val in = new File("frngs.txt")
  val out = new File("cels.txt")
  val convert =
    filter[String](!_.isEmpty) |>
      filter[String](!_.trim.startsWith("#"))
        .map(s => toCelsius(s.toDouble).toString)
  val io = processFromFileToFile(in, out, convert)
  unsafePerformIO(io)(Executors.newSingleThreadExecutor())
}

