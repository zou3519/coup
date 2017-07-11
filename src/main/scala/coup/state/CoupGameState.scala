package coup.state

import scala.collection.immutable.Queue

/**
  * Encapsulates the state of the game at any time.
  *
  * @param currentPlay Sequence of Actions that have just happened.
  * @param pendingStages What the game state is currently waiting on.
  *                     An action, resolving exchange, or discard
  */
class CoupGameState(
    val courtDeck: Seq[Character],
    val discardPile: Seq[Character],
    val coins: IndexedSeq[Int],
    val influences: IndexedSeq[Seq[Character]],
    val currentPlayer: PlayerT,
    val currentPlay: Seq[Action],
    val pendingStages: Queue[PendingStage],
    val ambassadorDeck: Seq[Action]) {

  def nextState(action: Action): CoupGameState = {
    require(Rules.isActionLegal(this, action))

    action match {
      case income: Income => applyIncome(income)
      case foreignAid: ForeignAid => applyForeignAid(foreignAid)
      case coup: Coup => applyCoup(coup)
      case tax: Tax => applyTax(tax)
      case exchange: Exchange => applyExchange(exchange)
      case steal: Steal => applySteal(steal)
      case assassinate: Assassinate => applyAssassinate(assassinate)
      case block: Block => applyBlock(block)
      case challenge: Challenge => applyChallenge(challenge)
      case noReaction: NoReaction => applyNoReaction(noReaction)
      case resolveExchange: ResolveExchange => applyResolveExchange(resolveExchange)
      case loseInfluence: LoseInfluence => applyLoseInfluence(loseInfluence)
      case proveInfluence: ProveInfluence => applyProveInfluence(proveInfluence)
    }
  }

  private def applyIncome(income: Income): CoupGameState = ???
  private def applyForeignAid(foreignAid: ForeignAid): CoupGameState = ???
  private def applyCoup(coup: Coup): CoupGameState = ???
  private def applyTax(tax: Tax): CoupGameState = ???
  private def applyExchange(exchange: Exchange): CoupGameState = ???
  private def applySteal(steal: Steal): CoupGameState = ???
  private def applyAssassinate(assassinate: Assassinate): CoupGameState = ???
  private def applyBlock(block: Block): CoupGameState = ???
  private def applyChallenge(challenge: Challenge): CoupGameState = ???
  private def applyNoReaction(noReaction: NoReaction): CoupGameState = ???
  private def applyResolveExchange(resolveExchange: ResolveExchange): CoupGameState = ???
  private def applyLoseInfluence(loseInfluence: LoseInfluence): CoupGameState = ???
  private def applyProveInfluence(proveInfluence: ProveInfluence): CoupGameState = ???
}
