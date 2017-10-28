import fpinscala.monoids.Monoid._
import fpinscala.testing.Gen
import fpinscala.testing.Gen._
import fpinscala.testing.Prop

val laws = List(
  monoidLaws(stringMonoid, Gen.smallInt.flatMap(stringN)),
  monoidLaws(listMonoid[Int], smallInt.flatMap(listOfN(_, smallInt))),
  monoidLaws(intAddition, smallInt),
  monoidLaws(intMultiplication, smallInt),
  monoidLaws(booleanOr, boolean),
  monoidLaws(booleanAnd, boolean)
)

laws foreach (Prop run _)