import fpinscala.laziness._
import fpinscala.laziness.Stream._

Stream(1, 2, 3).scanRight(0)(_ + _).toList
