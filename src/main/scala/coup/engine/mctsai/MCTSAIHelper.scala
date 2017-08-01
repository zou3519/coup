package coup.engine.mctsai

import coup.core._
import mcts.{MCTSPartialStateLike, MCTSStateLike}

import scala.util.Random

object MCTSAIHelper {

  implicit class CoupMCTSGameState(gs: CoupGameState) extends MCTSStateLike[Action] {

    override def currentPlayer: PlayerT = {
      assert(gs.influences.forall(_.nonEmpty))
      gs.pendingStages.head.player
    }

    override def numPlayers: PlayerT = 2

    override def copy: MCTSStateLike[Action] = new CoupMCTSGameState(gs.copy)

    override def moveInPlace(move: Action): Unit = gs.applyAction(move)

    override def legalMoves: IndexedSeq[Action] = {
      if (gs.influences.exists(_.isEmpty)) {
        Vector()
      } else
        Rules.legalActions(gs.toPartialGameState(currentPlayer))
    }

    override def simulate: IndexedSeq[Double] = {
      val state: CoupGameState = gs.copy
      while (state.pendingStages.nonEmpty) {
        val legalMoves = state.legalMoves
        val action = legalMoves(Random.nextInt(legalMoves.size))
        state.applyAction(action)
      }
      (state.influences(0).isEmpty, state.influences(1).isEmpty) match {
        case (true, false) => Vector(0.0, 1.0)
        case (false, true) => Vector(1.0, 0.0)
        case _ => ??? // TODO: exception
      }
    }
  }

  implicit class CoupMCTSPartialGameState(pgs: CoupPartialGameState)
      extends MCTSPartialStateLike[Action] {

    override def determinize: MCTSStateLike[Action] = pgs.determinize
  }
}
