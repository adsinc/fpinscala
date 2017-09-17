import fpinscala.state._
import fpinscala.state.State._

simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 3, 0))