package coup

import scala.collection.immutable.Seq

package object core {
  type PlayerT = Int
  type Cards = Seq[Character.EnumVal]
  type PlayerPiles[A] = IndexedSeq[A]
  val PlayerPiles = IndexedSeq
}
