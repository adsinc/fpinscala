import java.util.function.Predicate

import fpinscala.testing.{Gen, Prop}

Prop.run(Gen.opSeqProp)
