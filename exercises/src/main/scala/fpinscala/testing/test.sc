import java.util.function.Predicate

import fpinscala.testing.{Gen, Prop}

Prop.run(Prop.p5)


val p = (i: Int) => i < 3

val l =List(1, 2, 3, 4, 5)

l.takeWhile(p)

l.dropWhile(p)