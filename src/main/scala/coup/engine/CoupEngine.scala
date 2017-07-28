package coup.engine

import coup.core.CoupGameState

import scala.concurrent.Await
import scala.concurrent.duration._

object CoupEngine {

  def gameLoop(): Unit = {
    val gameState = CoupGameState.init
    val players = IndexedSeq(new Human, new Human)

    while (gameState.pendingStages.nonEmpty) {
      val currentPlayer = gameState.pendingStages.head.player
      val futureAction = players(currentPlayer).getAction(
        gameState.toPartialGameState(currentPlayer)
      )
      val action = Await.result(futureAction, Duration.Inf)
      gameState.applyAction(action)
    }

    // Confirm that someone has actually lost
    require(gameState.influences.exists(_.isEmpty))
    println(Describe.gameState(gameState))
    println("Game Over")
  }
}
