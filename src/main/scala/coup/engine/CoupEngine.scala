package coup.engine

import coup.core.CoupGameState

object CoupEngine {
  def gameLoop: Unit = {
    var gameState = CoupGameState.init
    var running = true
    val players = IndexedSeq(new Human, new Human)

    while (running) {
      val currentPlayer = gameState.pendingStages.front.player
      val action = players(currentPlayer).getAction(
        gameState.toPartialGameState(currentPlayer)
      )
      gameState = gameState.nextState(action)
    }
  }
}
