import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par._

val c = choiceMap(unit(2))(Map(1 -> unit("TRUE"), 2 -> unit("FALSE")))

run(Executors.newCachedThreadPool())(c)