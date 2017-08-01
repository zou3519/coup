package coup.engine

import coup.core.CoupGameState
import coup.engine.mctsai.MCTSAI

import scala.concurrent.Await
import scala.concurrent.duration._

object CoupEngine {

  def gameLoop(): Unit = {
    val gameState = CoupGameState.init(coins = Vector(2, 2))
    val players = IndexedSeq(new MCTSAI, new Human)

    while (gameState.pendingStages.nonEmpty) {
      val currentPlayer = gameState.pendingStages.head.player
      // println(Describe.gameState(gameState))
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
