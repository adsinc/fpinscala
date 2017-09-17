import java.util.concurrent.Executors

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par._

val c = map2ViaFlatMap(unit(10), unit(20))(_ * _)

run(Executors.newCachedThreadPool())(c)