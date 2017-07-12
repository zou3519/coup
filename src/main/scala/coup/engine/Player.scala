package coup.engine

import coup.core.{Action, CoupPartialGameState}

trait Player {
  def getAction(partialCoupGameState: CoupPartialGameState): Action
}
