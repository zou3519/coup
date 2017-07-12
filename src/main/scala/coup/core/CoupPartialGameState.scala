package coup.core

import scala.collection.immutable.Seq
import scala.collection.immutable.Queue

class CoupPartialGameState(
    val courtDeckSize: Int,
    val discardPile: PlayerPiles[Cards],
    val coins: PlayerPiles[Int],
    val myInfluences: Cards,
    val opponentInfluencesSize: Int,
    val currentPlay: Seq[Action],
    val pendingStages: Queue[PendingStage],
    val myAmbassadorDeck: Option[Cards],
    val me: PlayerT) {

  /* Generates a random CoupGameStage based on this partial state */
  def determinize: CoupGameState = ???
}
