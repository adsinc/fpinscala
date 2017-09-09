import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

val p = asyncF[Int, Int](a => a + 10)

run(Executors.newCachedThreadPool())(p(10)).get()

