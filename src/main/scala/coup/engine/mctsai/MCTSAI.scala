package coup.engine.mctsai

import coup.core.{Action, CoupPartialGameState, Rules}
import coup.engine.mctsai.MCTSAIHelper.CoupMCTSPartialGameState
import coup.engine.Player
import mcts.{ISMCTS, ISTreeNode}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class MCTSAI extends Player {
  val maxIter = 50000
  val maxMillis = 1000

  override def getAction(gameState: CoupPartialGameState): Future[Action] = Future {
    val currentNode = new ISTreeNode[Action](null, null, 2)
    val legalMoves = Rules.legalActions(gameState)
    legalMoves.length match {
      case 1 =>
        legalMoves.head
      case _ =>
        val bestChild: ISTreeNode[Action] = ISMCTS.UCTSearch(
          currentNode,
          new CoupMCTSPartialGameState(gameState),
          maxIter,
          maxMillis)
        // println(currentNode.selfAndKids(4))
        bestChild.move
    }
  }
}
