import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

import scala.util.Random

val as = List.fill(10)(Random.nextInt())

val pm = fold(as.tail.toIndexedSeq)(as.head)(math.max)

run(Executors.newCachedThreadPool())(pm).get