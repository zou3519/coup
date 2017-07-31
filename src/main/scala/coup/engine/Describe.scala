package coup.engine

import coup.core.{Action, CoupGameState, CoupPartialGameState}

object Describe {

  def gameState(gameState: CoupGameState): String = {
    "\u001b[2J\n" +
    "------------------ GameState dump ------------------\n" +
    "Deck: " + gameState.courtDeck + "\n" +
    "Discard: " + gameState.discardPile + "\n" +
    "Coins: " + gameState.coins + "\n" +
     "Influences: " + gameState.influences + "\n" +
    "Current Play: " + gameState.currentPlay + "\n" +
    "Pending: " + gameState.pendingStages + "\n" +
    "-----------------------------------------------------------"
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
    "Current player: " + partialGameState.me + "\n" +
    "-----------------------------------------------------------"
  }

  def action(action: Action): String = action.toString
}
