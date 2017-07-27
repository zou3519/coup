package coup.engine

import coup.core.CoupGameState

import scala.concurrent.Await
import scala.concurrent.duration._

object CoupEngine {

  def gameLoop(): Unit = {
    var gameState = CoupGameState.init
    val players = IndexedSeq(new Human, new Human)

    // TODO: rewrite in terms of future composition?
    while (true) {
      val currentPlayer = gameState.pendingStages.front.player
      val futureAction = players(currentPlayer).getAction(
        gameState.toPartialGameState(currentPlayer)
      )
      val action = Await.result(futureAction, Duration.Inf)
      gameState = gameState.nextState(action)
    }
  }
}
