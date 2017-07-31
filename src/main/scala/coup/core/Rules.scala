package coup.core

import scala.collection.mutable.ArrayBuffer

object Rules {

  def legalActions(partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    partialGameState.pendingStages.head match {
      case _: PrimaryAction => legalPrimaryActions(partialGameState)
      case _: Reaction => legalReactions(partialGameState)
      case _: ChooseExchange => legalChooseExchanges(partialGameState)
      case _: DiscardInfluence => legalDiscardInfluences(partialGameState)
      case _: ExamineInfluence => legalExamineInfluences(partialGameState)
    }
  }

  private def legalPrimaryActions(
      partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    val player = partialGameState.pendingStages.head.player
    val nextPlayer = (player + 1) % 2

    // Must coup
    if (partialGameState.coins(player) >= 10) {
      return Vector(Coup(player, nextPlayer))
    }

    val builder = IndexedSeq.newBuilder[Action]
    builder += (
      Income(player),
      ForeignAid(player),
      Tax(player),
      Exchange(player),
      Steal(player, nextPlayer)
    )

    if (partialGameState.coins(player) >= 3)
      builder += Assassinate(player, nextPlayer)

    if (partialGameState.coins(player) >= 7)
      builder += Coup(player, nextPlayer)

    builder.result
  }

  private def legalReactions(
      partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    val player = partialGameState.pendingStages.head.player
    val nextPlayer = (player + 1) % 2

    val builder = IndexedSeq.newBuilder[Action]
    val lastAction = partialGameState.currentPlay.last

    if (canBeBlocked(lastAction))
      builder += Block(player, nextPlayer)

    if (canBeChallenged(lastAction))
      builder += Challenge(player, nextPlayer)

    if (canBeReactedTo(lastAction))
      builder += DoNothing(player)

    builder.result
  }

  private def legalChooseExchanges(
      partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    require(partialGameState.myInfluences.length >= 3)

    val player = partialGameState.me
    val possibleReturnedCards = partialGameState.myInfluences.combinations(2)
    possibleReturnedCards.map(ResolveExchange(player, _)).toVector
  }

  private def legalDiscardInfluences(
      partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    val player = partialGameState.me
    partialGameState.myInfluences.map(LoseInfluence(player, _))
  }

  private def legalExamineInfluences(
      partialGameState: CoupPartialGameState): IndexedSeq[Action] = {
    require(partialGameState.currentPlay.last.isInstanceOf[Challenge])

    val player = partialGameState.me

    val playWithoutChallenge = partialGameState.currentPlay.dropRight(1).toVector
    val correctInfluences = sufficientInfluence(playWithoutChallenge)

    partialGameState.myInfluences.map(char => {
      if (correctInfluences.contains(char))
        ProveInfluence(player, char)
      else
        LoseInfluence(player, char)
    })
  }

  /*
   * Which characters have sufficient influence to perform the last action
   * in play.
   */
  private def sufficientInfluence(
      play: IndexedSeq[Action]): IndexedSeq[Character.EnumVal] = {
    require(play.size <= 2)

    if (play.size == 2)
      sufficientInfluenceToBlock(play.head)
    else
      Vector(sufficientInfluenceForPrimaryAction(play.head))
  }

  private def sufficientInfluenceForPrimaryAction(action: Action): Character.EnumVal = {
    action match {
      case _: Tax => Character.Duke
      case _: Exchange => Character.Ambassador
      case _: Steal => Character.Captain
      case _: Assassinate => Character.Assassin
      case _ => ??? // TODO: some requirement that we can't be here
    }
  }

  private def sufficientInfluenceToBlock(
      action: Action): IndexedSeq[Character.EnumVal] = {
    action match {
      case _: ForeignAid => Vector(Character.Duke)
      case _: Steal => Vector(Character.Ambassador, Character.Captain)
      case _: Assassinate => Vector(Character.Contessa)
      case _ => ??? // TODO: some requirement that we can't be here
    }
  }

  def isActionLegal(gameState: CoupGameState, action: Action): Boolean = {
    action match {

      /* Primary actions */
      case income: Income => isNonCoupPrimaryActionOK(gameState, income)
      case foreignAid: ForeignAid => isNonCoupPrimaryActionOK(gameState, foreignAid)
      case coup: Coup => isCoupOK(gameState, coup)

      case tax: Tax => isNonCoupPrimaryActionOK(gameState, tax)
      case exchange: Exchange => isNonCoupPrimaryActionOK(gameState, exchange)

      case assassinate: Assassinate => isAssassinateOK(gameState, assassinate)
      case steal: Steal => isStealOK(gameState, steal)

      /* Responses */
      case block: Block => isBlockOK(gameState, block)
      case challenge: Challenge => isChallengeOK(gameState, challenge)
      case noReaction: DoNothing => isNoReactionOK(gameState, noReaction)

      /* Resolutions */
      case resolveExchange: ResolveExchange =>
        isResolveExchangeOK(gameState, resolveExchange)
      case LoseInfluence(player, lostCharacter) =>
        isExamineInfluenceOK(gameState, player, lostCharacter)
      case ProveInfluence(player, provenCharacter) =>
        isExamineInfluenceOK(gameState, player, provenCharacter)
    }
  }

  def isNonCoupPrimaryActionOK(gameState: CoupGameState, action: Action): Boolean = {
    val player = action.player
    gameState.pendingStages.head match {
      case PrimaryAction(actionPlayer) =>
        player == actionPlayer && gameState.coins(player) < 10
      case _ => false
    }
  }

  def isCoupOK(gameState: CoupGameState, coup: Coup): Boolean = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    gameState.pendingStages.head match {
      case PrimaryAction(actionPlayer) =>
        player == actionPlayer &&
          gameState.coins(player) >= 7 &&
          player != targetPlayer
      case _ => false
    }
  }

  def isAssassinateOK(
      gameState: CoupGameState,
      assassinate: Assassinate): Boolean = {
    val player = assassinate.player
    val targetPlayer = assassinate.targetPlayer
    isNonCoupPrimaryActionOK(gameState, assassinate) &&
      gameState.coins(player) >= 3 &&
      player != targetPlayer
  }

  def isStealOK(gameState: CoupGameState, steal: Steal): Boolean = {
    val player = steal.player
    val targetPlayer = steal.targetPlayer
    isNonCoupPrimaryActionOK(gameState, steal) && player != targetPlayer
  }

  def isBlockOK(gameState: CoupGameState, block: Block): Boolean = {
    gameState.pendingStages.head match {
      case Reaction(actionPlayer) =>
        block.player == actionPlayer &&
          canBlockAction(block, gameState.currentPlay.lastOption)
      case _ => false
    }
  }

  def isChallengeOK(gameState: CoupGameState, challenge: Challenge): Boolean = {
    val player = challenge.player
    gameState.pendingStages.head match {
      case Reaction(actionPlayer) =>
        player == actionPlayer &&
          canChallengeAction(challenge, gameState.currentPlay.lastOption)
      case _ => false
    }
  }

  private def canBlockAction(block: Block, lastActionOpt: Option[Action]): Boolean = {
    lastActionOpt match {
      case None => false
      case Some(lastAction) =>
        canBeBlocked(lastAction) &&
        lastAction.player == block.targetPlayer &&
        block.targetPlayer != block.player
    }
  }

  private def canBeBlocked(action: Action): Boolean = {
    action match {
      case ForeignAid(_) => true
      case Assassinate(_, _) => true
      case Steal(_, _) => true
      case _ => false
    }
  }

  private def canChallengeAction(
      challenge: Challenge,
      lastActionOpt: Option[Action]): Boolean = {
    lastActionOpt match {
      case None => false
      case Some(lastAction) => canBeChallenged(lastAction)
    }
  }

  private def canBeChallenged(action: Action): Boolean = {
    action match {
      case _: Tax => true
      case _: Exchange => true
      case _: Steal => true
      case _: Assassinate => true
      case _: Block => true
      case _ => false
    }
  }

  def isNoReactionOK(gameState: CoupGameState, noReaction: DoNothing): Boolean = {
   gameState.pendingStages.head match {
      case Reaction(actionPlayer) =>
        noReaction.player == actionPlayer &&
          canNoReaction(noReaction, gameState.currentPlay.lastOption)
      case _ => false
    }
  }
  private def canNoReaction(noReaction: DoNothing, lastActionOpt: Option[Action]): Boolean = {
    lastActionOpt match {
      case None => false
      case Some(lastAction) =>
        lastAction.player != noReaction.player && canBeReactedTo(lastAction)
    }
  }

  private def canBeReactedTo(action: Action): Boolean = {
    canBeChallenged(action) || canBeBlocked(action)
  }

  def isResolveExchangeOK(
      gameState: CoupGameState,
      resolveExchange: ResolveExchange): Boolean =  {
    val player = resolveExchange.player
    gameState.pendingStages.head match {
      case ChooseExchange(exchgPlayer) =>

        // TODO: there has to be a better way to do this...
        val influences = gameState.influences(exchgPlayer).to[ArrayBuffer]
        influences --= resolveExchange.returnedCharacters
        val validReturn = influences.length == gameState.influences(exchgPlayer).length - 2

        (exchgPlayer == player) && validReturn

      case _ => false
    }
  }

  // TODO: maybe break this function up?
  def isExamineInfluenceOK(
      gameState: CoupGameState,
      player: PlayerT,
      revealedCharacter: Character.EnumVal): Boolean = {
    gameState.pendingStages.head match {
      case ExamineInfluence(contestedPlayer) =>
        player == contestedPlayer &&
          gameState.influences(player).contains(revealedCharacter)
      case DiscardInfluence(contestedPlayer) =>
        player == contestedPlayer &&
          gameState.influences(player).contains(revealedCharacter)
      case _ => false
    }
  }
}
