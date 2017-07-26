package coup.core

object Rules {

  def isActionLegal(gameState: CoupGameState, action: Action): Boolean = {
    action match {

      /* Primary actions */
      case income: Income => isNonCoupPrimaryActionOK(gameState, income)
      case foreignAid: ForeignAid => isNonCoupPrimaryActionOK(gameState, foreignAid)
      case coup: Coup => isCoupOK(gameState, coup)

      case tax: Tax => isNonCoupPrimaryActionOK(gameState, tax)
      case exchange: ChooseExchange => isNonCoupPrimaryActionOK(gameState, exchange)

      case assassinate: Assassinate => isAssassinateOK(gameState, assassinate)
      case steal: Steal => isStealOK(gameState, steal)

      /* Responses */
      case block: Block => isBlockOK(gameState, block)
      case challenge: Challenge => isChallengeOK(gameState, challenge)
      case noReaction: NoReaction => isNoReactionOK(gameState, noReaction)

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
    gameState.pendingStages.front match {
      case PrimaryAction(actionPlayer) =>
        player == actionPlayer && gameState.coins(player) < 10
      case _ => false
    }
  }

  def isCoupOK(gameState: CoupGameState, coup: Coup): Boolean = {
    val player = coup.player
    val targetPlayer = coup.targetPlayer
    gameState.pendingStages.front match {
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
    gameState.pendingStages.front match {
      case Reaction(actionPlayer) =>
        block.player == actionPlayer &&
          canBlockAction(block, gameState.currentPlay.lastOption)
      case _ => false
    }
  }

  def isChallengeOK(gameState: CoupGameState, challenge: Challenge): Boolean = {
    val player = challenge.player
    gameState.pendingStages.front match {
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
    val player = challenge.player
    val targetPlayer = challenge.targetPlayer
    lastActionOpt match {
      case None => false
      case Some(lastAction) => {
        lastAction match {
          case _: Income => false
          case _: ForeignAid => false
          case _: Coup => false
          case _: Challenge => false
          case _ => targetPlayer == lastAction.player && player != targetPlayer
        }
      }
    }
  }

  def isNoReactionOK(gameState: CoupGameState, noReaction: NoReaction): Boolean = {
   gameState.pendingStages.front match {
      case Reaction(actionPlayer) =>
        noReaction.player == actionPlayer &&
          canNoReaction(noReaction, gameState.currentPlay.lastOption)
      case _ => false
    }
  }
  private def canNoReaction(noReaction: NoReaction, lastActionOpt: Option[Action]): Boolean = {
    lastActionOpt match {
      case None => false
      case Some(lastAction) =>
        lastAction.player != noReaction.player && canBeReactedTo(lastAction)
    }
  }

  private def canBeReactedTo(action: Action): Boolean = {
    action match {
      case Coup(_, _) => false
      case Challenge(_, _) => false
      case Income(_) => false
      case _ => true
    }
  }

  def isResolveExchangeOK(
      gameState: CoupGameState,
      resolveExchange: ResolveExchange): Boolean =  {
    val player = resolveExchange.player
    gameState.pendingStages.front match {
      case ChooseExchange(exchgPlayer) =>
        exchgPlayer == player
        // TODO: validate exchanged cards
      case _ => false
    }
  }

  def isExamineInfluenceOK(
      gameState: CoupGameState,
      player: PlayerT,
      revealedCharacter: Character): Boolean = {
    gameState.pendingStages.front match {
      case ExamineInfluence(contestedPlayer) =>
        player == contestedPlayer &&
          gameState.influences(player).contains(revealedCharacter)
      case _ => false
    }
  }
}
