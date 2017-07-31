package coup.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Initializer object
  */
object CoupGameState {
  def init(coins: Vector[Int] = Vector(2, 2)): CoupGameState = {
    val numPlayers = 2

    // Generate court deck and influences
    val allCards = Character.characters.flatMap(x => Seq(x, x, x))
    val shuffledCards = Random.shuffle(allCards).to[ArrayBuffer]

    val (dealtCards, courtDeck) = shuffledCards.splitAt(numPlayers * 2)
    val influences = dealtCards.grouped(2).to[ArrayBuffer]

    new CoupGameState(
      new CourtDeck(courtDeck),              // deck
      mu.PlayerPiles.fill(numPlayers)(ArrayBuffer()), // discardPile
      coins.to[ArrayBuffer],     // coins
      influences,             // influences
      ArrayBuffer(),                  // currentPlay
      mutable.Queue(PrimaryAction(0)) // pendingStages
    )
  }
  def initWith(
      playerCards: Cards,
      otherPlayerCards: Cards,
      coins: Vector[Int] = Vector(2, 2)): CoupGameState = {
    val numPlayers = 2

    val allCards = Character.characters.flatMap(x => Seq(x, x, x)).to[ArrayBuffer]
    allCards --= playerCards
    allCards --= otherPlayerCards
    val influences = ArrayBuffer(playerCards, otherPlayerCards).map(_.to[ArrayBuffer])

    new CoupGameState(
      new CourtDeck(allCards),              // deck
      mu.PlayerPiles.fill(numPlayers)(ArrayBuffer()), // discardPile
      coins.to[ArrayBuffer],     // coins
      influences,             // influences
      ArrayBuffer(),                  // currentPlay
      mutable.Queue(PrimaryAction(0)) // pendingStages
    )
  }
}

/**
  * Encapsulates the state of the game at any time.
  *
  * @param _currentPlay Sequence of Actions that have just happened.
  * @param _pendingStages What the game state is currently waiting on.
  *                       An action, resolving exchange, or discard.
  *                       When this is empty, the game is over.
  */
class CoupGameState(
    private val _courtDeck: CourtDeck,
    private val _discardPile: mu.PlayerPiles[mu.Cards],
    private val _coins: mu.PlayerPiles[Int],
    private val _influences: mu.PlayerPiles[mu.Cards],
    private val _currentPlay: ArrayBuffer[Action],
    private val _pendingStages: mutable.Queue[PendingStage]) {

  /* (slow) Accessors */
  def courtDeck: Cards = _courtDeck.toVector
  def discardPile: PlayerPiles[Cards] = _discardPile.map(_.toVector).toVector
  def coins: PlayerPiles[Int] = _coins.toVector
  def influences: PlayerPiles[Cards] = _influences.map(_.toVector).toVector
  def currentPlay: Vector[Action] = _currentPlay.toVector
  def pendingStages: Vector[PendingStage] = _pendingStages.toVector

  def copy: CoupGameState = {
    new CoupGameState(
      _courtDeck.clone,
      _discardPile.map(_.clone),
      _coins.clone,
      _influences.map(_.clone),
      _currentPlay.clone,
      _pendingStages.clone
    )
  }

  /* From the view of a player */
  def toPartialGameState(player: PlayerT): CoupPartialGameState = {

    new CoupPartialGameState(
      _courtDeck.length,
      _discardPile.map(_.toVector).toVector,
      _coins.toVector,
      _influences(player).toVector,
      _influences(nextPlayer(player)).size, // assumes 2p
      _currentPlay.toVector,
      _pendingStages.toVector,
      player
    )
  }

  /* Apply the action */
  def applyAction(action: Action): Unit = {
    require(Rules.isActionLegal(this, action))

    _pendingStages.dequeue()
    _currentPlay.append(action)

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
    nextTurn()
  }

  private def applyCoup(coup: Coup): Unit = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    _coins(player) -= 7
    _pendingStages.enqueue(DiscardInfluence(targetPlayer))
  }

  private def applyReactableAction(action: Action): Unit = {
    _pendingStages.enqueue(Reaction(nextPlayer(action.player)))
  }

  private def applyAssassinate(assassinate: Assassinate): Unit = {
    _coins(assassinate.player) -= 3
    applyReactableAction(assassinate)
  }

  private def applyChallenge(challenge: Challenge): Unit = {
    _pendingStages.enqueue(ExamineInfluence(nextPlayer(challenge.player)))
  }


  private def applyDoNothing(doNothing: DoNothing): Unit = {
    val actionOpt = actionToResolve(_currentPlay.dropRight(1), topActionSucceeds = true)
    actionOpt match {
      case Some(action) => resolveAction(action)
      case None => nextTurn()
    }
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
      // Top action should be either ProveInfluence, Challenge, or Block.
      // When it succeeds, the one below it should fail.

      if (pendingAction.isInstanceOf[Challenge] &&
          actionStack.last.isInstanceOf[Assassinate]) {
        // If assassinate is successfully challenged, coins are returned
        _coins(actionStack.last.player) += 3
      }

      actionStack.trimEnd(1)
    }

    // next pending action succeeds
    actionToResolve(actionStack, topActionSucceeds = true)
  }


  private def applyResolveExchange(resolveExchange: ResolveExchange): Unit = ???

  // TODO: this is really messy. cleanup sometime.
  private def applyLoseInfluence(loseInfluence: LoseInfluence): Unit = {
    val player = loseInfluence.player

    _influences(player) -= loseInfluence.lostCharacter
    _discardPile(player) += loseInfluence.lostCharacter
    if (_influences(player).isEmpty) {
      endGame()
      return
    }

    // two cases: 1. challenge happened or 2. player coup'ed or assassinated

    // case 1: challenge happened
    val prevAction = _currentPlay(_currentPlay.size - 2)
    if (prevAction.isInstanceOf[Challenge] ||
        prevAction.isInstanceOf[ProveInfluence]) {

      // figure out which actions go through
      val actionOpt = actionToResolve(_currentPlay.dropRight(1), topActionSucceeds = true)
      actionOpt match {
        case Some(action) => resolveAction(action)
        case None => nextTurn()
      }

    // case 2: player coup'ed or assassinated
    } else {
      nextTurn()
    }
  }

  private def endGame(): Unit = {
    require(_influences.exists(_.isEmpty))

    _currentPlay.clear()
    _pendingStages.clear()
  }

  private def applyProveInfluence(proveInfluence: ProveInfluence): Unit = {
    val player = proveInfluence.player

    // Shuffle old influence into deck and get a new one
    val oldInfluence = proveInfluence.provenCharacter
    val newInfluence = _courtDeck.swapWithRandomCard(oldInfluence)
    _influences(player) -= oldInfluence
    _influences(player) += newInfluence

    // Player who instigated challenge loses an influence
    val otherPlayer = nextPlayer(proveInfluence.player)
    _pendingStages.enqueue(DiscardInfluence(otherPlayer))
  }

  private def resolveAction(pendingAction: Action): Unit = {
    pendingAction match {
      case tax: Tax => resolveTax(tax)
      case foreignAid: ForeignAid => resolveForeignAid(foreignAid)
      case steal: Steal => resolveSteal(steal)
      case assassinate: Assassinate => resolveAssassinate(assassinate)
      case _ => ??? // TODO: exception here
    }
  }

  private def nextTurn(): Unit = {
    val otherPlayer = nextPlayer(_currentPlay.head.player)
    _currentPlay.clear()
    _pendingStages.enqueue(PrimaryAction(otherPlayer))
  }

  private def resolveForeignAid(foreignAid: ForeignAid): Unit = {
    _coins(foreignAid.player) += 2
    nextTurn()
  }

  private def resolveTax(tax: Tax): Unit = {
    _coins(tax.player) += 3
    nextTurn()
  }

  private def resolveSteal(steal: Steal): Unit = {
    val targetPlayer = steal.targetPlayer
    val player = steal.player
    val stolenCoins = if (_coins(targetPlayer) < 2) _coins(targetPlayer) else 2
    _coins(player) += stolenCoins
    _coins(targetPlayer) -= stolenCoins
    nextTurn()
  }

  private def resolveAssassinate(assassinate: Assassinate): Unit = {
    _pendingStages.enqueue(DiscardInfluence(assassinate.targetPlayer))
  }
}
