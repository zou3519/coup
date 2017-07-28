package coup.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Initializer object
  */
object CoupGameState {
  def init: CoupGameState = {
    val numPlayers = 2

    // Generate court deck and influences
    val allCards = Character.characters.flatMap(x => Seq(x, x, x))
    val shuffledCards = Random.shuffle(allCards).to[ArrayBuffer]

    val (dealtCards, courtDeck) = shuffledCards.splitAt(numPlayers * 2)
    val influences = dealtCards.grouped(2).to[ArrayBuffer]

    new CoupGameState(
      courtDeck,              // deck
      mu.PlayerPiles.fill(numPlayers)(ArrayBuffer()), // discardPile
      mu.PlayerPiles.fill(numPlayers)(2),     // coins
      influences,             // influences
      ArrayBuffer(),                  // currentPlay
      mutable.Queue(PrimaryAction(0)),// pendingStages
      None                    // ambassadorDeck
    )
  }
}

/**
  * Encapsulates the state of the game at any time.
  *
  * @param _currentPlay Sequence of Actions that have just happened.
  * @param _pendingStages What the game state is currently waiting on.
  *                     An action, resolving exchange, or discard
  */
class CoupGameState(
    private val _courtDeck: mu.Cards,
    private val _discardPile: mu.PlayerPiles[mu.Cards],
    private val _coins: mu.PlayerPiles[Int],
    private val _influences: mu.PlayerPiles[mu.Cards],
    private val _currentPlay: ArrayBuffer[Action],
    private val _pendingStages: mutable.Queue[PendingStage],
    private var _ambassadorDeck: Option[mu.Cards]) {

  /* (slow) Accessors */
  def coins: PlayerPiles[Int] = _coins.toVector
  def influences: PlayerPiles[Cards] = _influences.map(_.toVector).toVector
  def currentPlay: Vector[Action] = _currentPlay.toVector
  def pendingStages: Vector[PendingStage] = _pendingStages.toVector
  def ambassadorDeck: Option[Cards] = _ambassadorDeck.map(_.toVector)

  /* From the view of a player */
  def toPartialGameState(player: PlayerT): CoupPartialGameState = {

    // Hide ambassador deck if necessary
    val showAmbassadorDeck = _pendingStages.front match {
      case ChooseExchange(requestedPlayer) =>
        if (requestedPlayer == player) _ambassadorDeck else None
      case _ => None
    }

    new CoupPartialGameState(
      _courtDeck.length,
      _discardPile.map(_.toVector).toVector,
      _coins.toVector,
      _influences(player).toVector,
      _influences(nextPlayer(player)).size, // assumes 2p
      _currentPlay.toVector,
      _pendingStages.toVector,
      showAmbassadorDeck.map(_.toVector),
      player
    )
  }

  /* Apply the action */
  def applyAction(action: Action): Unit = {
    require(Rules.isActionLegal(this, action))

    _pendingStages.dequeue()

    action match {
      case income: Income => applyIncome(income)
      case foreignAid: ForeignAid => applyReactableAction(foreignAid)
      case coup: Coup => applyCoup(coup)
      case tax: Tax => applyReactableAction(tax)
      case exchange: Exchange => applyReactableAction(exchange)
      case steal: Steal => applyReactableAction(steal)
      case assassinate: Assassinate => applyAssassinate(assassinate)
      case block: Block => applyReactableAction(block)
      case challenge: Challenge => applyChallenge(challenge)
      case noReaction: DoNothing => applyDoNothing(noReaction)
      case resolveExchange: ResolveExchange => applyResolveExchange(resolveExchange)
      case loseInfluence: LoseInfluence => applyLoseInfluence(loseInfluence)
      case proveInfluence: ProveInfluence => applyProveInfluence(proveInfluence)
    }
  }

  private def nextPlayer(player: PlayerT) = {
    (player + 1) % 2
  }

  private def applyIncome(income: Income): Unit = {
    val player = income.player
    _coins(player) += 1
    _pendingStages.enqueue(PrimaryAction(nextPlayer(player)))
  }

  private def applyCoup(coup: Coup): Unit = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    _coins(player) -= 7
    _pendingStages.enqueue(
      DiscardInfluence(targetPlayer),
      PrimaryAction(nextPlayer(player)))
  }

  private def applyReactableAction(action: Action): Unit = {
    _currentPlay.append(action)
    _pendingStages.enqueue(Reaction(nextPlayer(action.player)))
  }

  private def applyAssassinate(assassinate: Assassinate): Unit = {
    _coins(assassinate.player) -= 3
    applyReactableAction(assassinate)
  }

  private def applyChallenge(challenge: Challenge): Unit = {
    _currentPlay.append(challenge)
    _pendingStages.enqueue(ExamineInfluence(nextPlayer(challenge.player)))
  }


  private def applyDoNothing(doNothing: DoNothing): Unit = {
    val nextPlayer = nextPlayer(_currentPlay.head.player)
    _pendingStages.enqueue(PrimaryAction(nextPlayer))
    val action = actionToResolve(_currentPlay, topActionSucceeds = true)
    action.foreach(resolveAction)
  }

  // always clears actionStack
  private def actionToResolve(
      actionStack: ArrayBuffer[Action],
      topActionSucceeds: Boolean): Option[Action] = {
    if (actionStack.isEmpty) {
      return None
    }

    // pop element off stack
    val pendingAction = actionStack.last
    actionStack.trimEnd(1)

    if (topActionSucceeds && actionStack.isEmpty) {
      return Some(pendingAction)
    }

    if (topActionSucceeds) {
      // next pending action is cancelled out
      actionStack.trimEnd(1)
    }

    // next pending action succeeds
    actionToResolve(actionStack, topActionSucceeds = true)
  }


  private def applyResolveExchange(resolveExchange: ResolveExchange): Unit = ???

  private def applyLoseInfluence(loseInfluence: LoseInfluence): Unit = {
    val player = loseInfluence.player
    _influences(player) -= loseInfluence.lostCharacter

    // two cases: 1. challenge was conceded/lost or 2. player coup'ed or assassinated

    // case 1: challenge was lost
    if (_currentPlay.last.isInstanceOf[Challenge]) {
      val action = actionToResolve(_currentPlay, topActionSucceeds = true)
      action.foreach(resolveAction)
      return
    }

    // case 2: player was coup'ed or assassinated
    _currentPlay.clear()
  }

  private def applyProveInfluence(proveInfluence: ProveInfluence): Unit = ???

  private def resolveAction(pendingAction: Action): Unit = {
    pendingAction match {
      case tax: Tax => resolveTax(tax)
      case foreignAid: ForeignAid => resolveForeignAid(foreignAid)
      case steal: Steal => resolveSteal(steal)
      case assassinate: Assassinate => resolveAssassinate(assassinate)
      case _ => ??? // Should not be here...
    }
  }

  private def resolveForeignAid(foreignAid: ForeignAid): Unit = {
    _coins(foreignAid.player) += 2
  }

  private def resolveTax(tax: Tax): Unit = {
    _coins(tax.player) += 3
  }

  private def resolveSteal(steal: Steal): Unit = {
    val targetPlayer = steal.targetPlayer
    val player = steal.player
    val stolenCoins = if (_coins(targetPlayer) < 2) _coins(targetPlayer) else 2
    _coins(player) += stolenCoins
    _coins(targetPlayer) += stolenCoins
  }

  private def resolveAssassinate(assassinate: Assassinate): Unit = ???



}
