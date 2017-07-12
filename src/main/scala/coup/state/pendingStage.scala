package coup.state

sealed trait PendingStage {
  val player: PlayerT
}

case class PrimaryAction(player: PlayerT) extends PendingStage
case class Reaction(player: PlayerT) extends PendingStage
case class Exchange(player: PlayerT) extends PendingStage
case class DiscardInfluence(player: PlayerT) extends PendingStage
case class ExamineInfluence(player: PlayerT) extends PendingStage
