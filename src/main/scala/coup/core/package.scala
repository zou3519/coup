package coup

import collection.immutable.Vector
import collection.mutable.ArrayBuffer

package object core {
  type PlayerT = Int
  type Cards = Vector[Character.EnumVal]
  type PlayerPiles[A] = Vector[A]
  val PlayerPiles = Vector
  type Card = Character.EnumVal

  object mu {
    type Cards = ArrayBuffer[Character.EnumVal]
    type PlayerPiles[A] = ArrayBuffer[A]
    val PlayerPiles = ArrayBuffer
  }
}
