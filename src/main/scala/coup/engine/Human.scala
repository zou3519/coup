package coup.engine

import scala.io.StdIn._
import coup.core.{Action, Coup, CoupPartialGameState, Income}

class Human extends Player {
  override def getAction(partialCoupGameState: CoupPartialGameState): Action = {
    val player = partialCoupGameState.me
    val nextPlayer = (player + 1) % 2
    var done = false

    PrettyPrinter.printPartialGameState(partialCoupGameState)
    val option = readLine(s"Player $player's turn\n1. Income\n2. Coup\n>> ")
    option.stripLineEnd match {
      case "2" => Coup(player, nextPlayer)
      case "1" => Income(player)
    }
  }
}
