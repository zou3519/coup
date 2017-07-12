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

  def copy(
      courtDeck: Seq[Character] = courtDeck,
      discardPile: Seq[Character] = discardPile,
      coins: IndexedSeq[Int] = coins,
      influences: IndexedSeq[Seq[Character]] = influences,
      currentPlay: Seq[Action] = currentPlay,
      pendingStages: Queue[PendingStage] = pendingStages,
      ambassadorDeck: Seq[Action] = ambassadorDeck): CoupGameState = {
    new CoupGameState(
      courtDeck,
      discardPile,
      coins,
      influences,
      currentPlay,
      pendingStages,
      ambassadorDeck
    )
  }

  private def nextPlayer(player: PlayerT) = {
    (player + 1) % 2
  }

  private def applyIncome(income: Income): CoupGameState = {
    val player = income.player
    val newIncome = coins(player) + 1
    val newPendingStages = pendingStages.drop(1) :+ Reaction(nextPlayer(player))

    copy(
      coins = coins.updated(player, newIncome),
      currentPlay = currentPlay :+ income,
      pendingStages = newPendingStages
    )
  }

  private def applyForeignAid(foreignAid: ForeignAid): CoupGameState = ???

  private def applyCoup(coup: Coup): CoupGameState = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    val newPendingStages =
      pendingStages.drop(1) :+
        DiscardInfluence(targetPlayer) :+
        PrimaryAction(nextPlayer(player))
    val newIncome = coins(player) - 7

    copy(
      coins = coins.updated(player, newIncome),
      currentPlay = currentPlay :+ coup,
      pendingStages = newPendingStages
    )
  }

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
