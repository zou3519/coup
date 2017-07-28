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
case class ResolveExchange(player: PlayerT, returnedCharacters: Seq[Character.EnumVal]) extends Action
case class LoseInfluence(player: PlayerT, lostCharacter: Character.EnumVal) extends Action
case class ProveInfluence(player: PlayerT, provenCharacter: Character.EnumVal) extends Action
