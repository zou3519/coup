package coup.core

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class CoupPartialGameState(
    val courtDeckSize: Int,
    val discardPile: PlayerPiles[Cards],
    val coins: PlayerPiles[Int],
    val myInfluences: Cards,
    val opponentInfluencesSize: Int,
    val currentPlay: Seq[Action],
    val pendingStages: Vector[PendingStage],
    val me: PlayerT) {

  /* Generates a random CoupGameStage based on this partial state */
  def determinize: CoupGameState = {

    // Figure out what cards are not shown
    val hiddenCards = Character.characters.flatMap(x => ArrayBuffer(x, x, x)).to[ArrayBuffer]
    hiddenCards --= myInfluences
    discardPile.foreach(hiddenCards --= _)

    // Split hidden cards among the deck and the opponent's hand
    val courtDeck = new CourtDeck(hiddenCards)
    val opponentsInfluence = (1 to opponentInfluencesSize).map(_ => courtDeck.takeRandomCard())

    // Construct influences
    val influences = me match {
      case 0 => ArrayBuffer(myInfluences, opponentsInfluence).map(_.to[ArrayBuffer])
      case 1 => ArrayBuffer(opponentsInfluence, myInfluences).map(_.to[ArrayBuffer])
    }

    new CoupGameState(
      courtDeck,    // deck
      discardPile.map(_.to[ArrayBuffer]).to[ArrayBuffer],  // discardPile
      coins.to[ArrayBuffer],        // coins
      influences,   // influences
      currentPlay.to[ArrayBuffer],  // current play
      pendingStages.to[mutable.Queue] // pending stages
    )
  }
}
