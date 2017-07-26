package coup.core

import scala.collection.immutable.Queue
import scala.collection.immutable.Seq
import scala.util.Random

/**
  * Initializer object
  */
object CoupGameState {
  def init: CoupGameState = {
    val numPlayers = 2

    // Generate court deck and influences
    val allCards = Character.characters.flatMap(x => Seq(x, x, x))
    val shuffledCards = Random.shuffle(allCards)

    val (dealtCards, courtDeck) = shuffledCards.splitAt(numPlayers * 2)
    val influences = dealtCards.grouped(2).toIndexedSeq

    new CoupGameState(
      courtDeck,              // deck
      PlayerPiles.fill(numPlayers)(Seq()), // discardPile
      PlayerPiles.fill(numPlayers)(2),     // coins
      influences,             // influences
      Seq(),                  // currentPlay
      Queue(PrimaryAction(0)),// pendingStages
      None                    // ambassadorDeck
    )
  }
}

/**
  * Encapsulates the state of the game at any time.
  *
  * @param currentPlay Sequence of Actions that have just happened.
  * @param pendingStages What the game state is currently waiting on.
  *                     An action, resolving exchange, or discard
  */
class CoupGameState(
    val courtDeck: Cards,
    val discardPile: PlayerPiles[Cards],
    val coins: PlayerPiles[Int],
    val influences: PlayerPiles[Cards],
    val currentPlay: Seq[Action],
    val pendingStages: Queue[PendingStage],
    val ambassadorDeck: Option[Cards]) {

  /* Shortcut to copying this object */
  def copy(
            courtDeck: Cards = courtDeck,
            discardPile: PlayerPiles[Cards] = discardPile,
            coins: PlayerPiles[Int] = coins,
            influences: PlayerPiles[Cards] = influences,
            currentPlay: Seq[Action] = currentPlay,
            pendingStages: Queue[PendingStage] = pendingStages,
            ambassadorDeck: Option[Cards] = ambassadorDeck): CoupGameState = {
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

  /* From the view of a player */
  def toPartialGameState(player: PlayerT): CoupPartialGameState = {

    // Hide ambassador deck if necessary
    val showAmbassadorDeck = pendingStages.front match {
      case ChooseExchange(requestedPlayer) =>
        if (requestedPlayer == player) ambassadorDeck else None
      case _ => None
    }

    new CoupPartialGameState(
      courtDeck.length,
      discardPile,
      coins,
      influences(player),
      influences(nextPlayer(player)).size, // assumes 2p
      currentPlay,
      pendingStages,
      showAmbassadorDeck,
      player
    )
  }

  /* Apply the action */
  def nextState(action: Action): CoupGameState = {
    require(Rules.isActionLegal(this, action))

    action match {
      case income: Income => applyIncome(income)
      case foreignAid: ForeignAid => applyForeignAid(foreignAid)
      case coup: Coup => applyCoup(coup)
      case tax: Tax => applyTax(tax)
      case exchange: ChooseExchange => applyExchange(exchange)
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

  private def nextPlayer(player: PlayerT) = {
    (player + 1) % 2
  }

  private def applyIncome(income: Income): CoupGameState = {
    val player = income.player
    val newIncome = coins(player) + 1
    val newPendingStages = pendingStages.drop(1) :+ PrimaryAction(nextPlayer(player))

    copy(
      coins = coins.updated(player, newIncome),
      currentPlay = currentPlay,
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
  private def applyExchange(exchange: ChooseExchange): CoupGameState = ???
  private def applySteal(steal: Steal): CoupGameState = ???
  private def applyAssassinate(assassinate: Assassinate): CoupGameState = ???
  private def applyBlock(block: Block): CoupGameState = ???
  private def applyChallenge(challenge: Challenge): CoupGameState = ???
  private def applyNoReaction(noReaction: NoReaction): CoupGameState = ???
  private def applyResolveExchange(resolveExchange: ResolveExchange): CoupGameState = ???

  private def applyLoseInfluence(loseInfluence: LoseInfluence): CoupGameState = {
    val player = loseInfluence.player
    val newInfluences = influences(player).diff(Seq(loseInfluence.lostCharacter))

    copy(
      influences = influences.updated(player, newInfluences),
      currentPlay = Seq(),
      pendingStages = pendingStages.drop(1)
    )
  }

  private def applyProveInfluence(proveInfluence: ProveInfluence): CoupGameState = ???
}
