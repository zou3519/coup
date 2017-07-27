package coup.engine

import coup.core.{Action, CoupPartialGameState}

import scala.concurrent.Future

trait Player {
  def getAction(partialCoupGameState: CoupPartialGameState): Future[Action]
}
