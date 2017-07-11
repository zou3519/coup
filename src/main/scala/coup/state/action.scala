package coup.state

sealed trait Action {
  val player: PlayerT
}

/* Basic actions */
case class Income(player: PlayerT) extends Action
case class ForeignAid(player: PlayerT) extends Action
case class Coup(player: PlayerT, targetPlayer: PlayerT) extends Action

/* Actions requiring influence */
case class Tax(player: PlayerT) extends Action
case class Exchange(player: PlayerT) extends Action
case class Steal(player: PlayerT, targetPlayer: PlayerT) extends Action
case class Assassinate(player: PlayerT, targetPlayer: PlayerT) extends Action

/* Responses */
case class Block(player: PlayerT, targetPlayer: PlayerT) extends Action
case class Challenge(player: PlayerT, targetPlayer: PlayerT) extends Action

/* Player confirms that they do not want to react to reactable action */
case class NoReaction(player: PlayerT) extends Action

/* Resolutions */
case class ResolveExchange(player: PlayerT, returnedCharacters: Seq[Character]) extends Action
case class LoseInfluence(player: PlayerT, lostCharacter: Character) extends Action
case class LoseDoubleInfluence(player: PlayerT, lostCharacters: Seq[Character]) extends Action
case class ProveInfluence(player: PlayerT, provenCharacter: Character) extends Action