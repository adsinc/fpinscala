import fpinscala.testing.{Gen, Prop}

Prop.run(Gen.sortedProp)

List(8, 8, 8, 5).sorted