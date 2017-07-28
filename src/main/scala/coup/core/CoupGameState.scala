package coup.core

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Queue}
//import scala.collection.immutable.Seq
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

  private def nextPlayer(player: PlayerT) = {
    (player + 1) % 2
  }

  private def applyIncome(income: Income): Unit = {
    val player = income.player
    _coins(player) += 1
    _pendingStages.dequeue()
    _pendingStages.enqueue(PrimaryAction(nextPlayer(player)))
  }

  private def applyForeignAid(foreignAid: ForeignAid): Unit = ???

  private def applyCoup(coup: Coup): Unit = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    _coins(player) -= 7
    _pendingStages.dequeue()
    _pendingStages.enqueue(
      DiscardInfluence(targetPlayer),
      PrimaryAction(nextPlayer(player)))
  }

  private def applyTax(tax: Tax): Unit = ???
  private def applyExchange(exchange: Exchange): Unit = ???
  private def applySteal(steal: Steal): Unit = ???
  private def applyAssassinate(assassinate: Assassinate): Unit = ???
  private def applyBlock(block: Block): Unit = ???
  private def applyChallenge(challenge: Challenge): Unit = ???
  private def applyNoReaction(noReaction: NoReaction): Unit = ???
  private def applyResolveExchange(resolveExchange: ResolveExchange): Unit = ???

  private def applyLoseInfluence(loseInfluence: LoseInfluence): Unit = {
    val player = loseInfluence.player
    _influences(player) -= loseInfluence.lostCharacter
    _currentPlay.clear()
    _pendingStages.dequeue()
  }

  private def applyProveInfluence(proveInfluence: ProveInfluence): Unit = ???
}
