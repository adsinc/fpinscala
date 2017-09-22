import fpinscala.state.RNG.Simple
import fpinscala.testing.Gen._

choose(1, 10).sample.run(Simple(1012))


