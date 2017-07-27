package coup.engine

import coup.core.Action

import scala.concurrent.Future
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global

object Prompt {

  def promptAction(actions: IndexedSeq[Action]): Future[Action] = Future {
    printActions(actions)
    val choice = Iterator.continually(StdIn.readLine("> "))
      .find(input => validChoice(input, actions))
    actions(choice.get.toInt - 1)
  }

  private def printActions(actions: IndexedSeq[Action]): Unit = {
    actions.zipWithIndex
      .foreach {
        case (action, index) => println(s"${index + 1}. ${Describe.action(action)}")
      }
  }

  private def validChoice(choice: String, actions: IndexedSeq[Action]): Boolean = {
    toInt(choice) match {
      case Some(value) => value >= 1 && value <= actions.size
      case None => false
    }
  }

  private def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }
}
