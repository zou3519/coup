package coup.engine

import coup.core.{Action, Coup, CoupPartialGameState, Income}

import scala.concurrent.Future

class Human extends Player {

  override def getAction(partialCoupGameState: CoupPartialGameState): Future[Action] = {
    val player = partialCoupGameState.me
    val nextPlayer = (player + 1) % 2

    println(Describe.partialGameState(partialCoupGameState))
    val possibleActions = IndexedSeq(Income(player), Coup(player, nextPlayer))
    Prompt.promptAction(possibleActions)
  }
}
