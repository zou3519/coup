package coup.core

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

/* Resolutions. After these actions, the original action is resolved or blocked. */
case class DoNothing(player: PlayerT) extends Action
case class LoseInfluence(player: PlayerT, lostCharacter: Card) extends Action
case class ProveInfluence(player: PlayerT, provenCharacter: Card) extends Action

/*
 * TODO: other player really shouldn't be able to know what was returned...
 * however, the player needs a way to tell the game state about what it is returning.
 * such information would need to be censored in a GameState -> PartialGameState conversion
 */
case class ResolveExchange(player: PlayerT, returnedCharacters: Vector[Card]) extends Action
