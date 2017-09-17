import java.util.concurrent.Executors

import fpinscala.parallelism.Par._

val as = List("Hello java", "Ololo", "London is the capital")

run(Executors.newCachedThreadPool())(countWords(as)).get