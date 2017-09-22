import fpinscala.state.RNG.Simple
import fpinscala.testing.Gen._

choose(1, 10).sample.run(Simple(1012))

unit(10).sample.run(Simple(100))

boolean.sample.run(Simple(10))

listOfN(5, boolean).sample.run(Simple(101))