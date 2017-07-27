package coup.engine

import coup.core.{Action, CoupGameState, CoupPartialGameState}

object Describe {

  def printGameState(gameState: CoupGameState): Unit = {

  }

  def partialGameState(partialGameState: CoupPartialGameState): String = {
    "\u001b[2J\n" +
    "------------------ PartialGameState dump ------------------\n" +
    "Deck: " + partialGameState.courtDeckSize + "\n" +
    "Discard: " + partialGameState.discardPile + "\n" +
    "Coins: " + partialGameState.coins + "\n" +
     "Influences: " + partialGameState.myInfluences + "\n" +
    "Opponent's Influence: " + partialGameState.opponentInfluencesSize + "\n" +
    "Current Play: " + partialGameState.currentPlay + "\n" +
    "Pending: " + partialGameState.pendingStages + "\n" +
    "Ambassador deck: " + partialGameState.myAmbassadorDeck + "\n" +
    "Current player: " + partialGameState.me + "\n" +
    "-----------------------------------------------------------"
  }

  def action(action: Action): String = action.toString
}
