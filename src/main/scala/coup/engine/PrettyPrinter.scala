package coup.engine

import coup.core.{CoupGameState, CoupPartialGameState}

object PrettyPrinter {

  def printGameState(gameState: CoupGameState): Unit = {

  }

  def printPartialGameState(partialGameState: CoupPartialGameState): Unit = {
    println("------------------ PartialGameState dump ------------------")
    println("Deck: " + partialGameState.courtDeckSize)
    println("Discard: " + partialGameState.discardPile)
    println("Coins: " + partialGameState.coins)
    println("Influences: " + partialGameState.myInfluences)
    println("Opponent's Influence: " + partialGameState.opponentInfluencesSize)
    println("Current Play: " + partialGameState.currentPlay)
    println("Pending: " + partialGameState.pendingStages)
    println("Ambassador deck: " + partialGameState.myAmbassadorDeck)
    println("Current player: " + partialGameState.me)
    println("-----------------------------------------------------------")
  }
}
