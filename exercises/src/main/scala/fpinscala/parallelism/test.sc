import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

val p = asyncF[Int, Int](a => a + 10)

run(Executors.newCachedThreadPool())(p(10)).get()

val pm = parMap(List.fill(1000)(10))(_ + 1)

run(Executors.newCachedThreadPool())(pm).get