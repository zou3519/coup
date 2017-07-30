package coup.core

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class CourtDeck(private val arrayBuffer: ArrayBuffer[Card]) {

  def swapWithRandomCard(card: Card): Card = {
    val randIndex = Random.nextInt(arrayBuffer.size)
    val result = arrayBuffer(randIndex)
    arrayBuffer(randIndex) = card
    result
  }

  def takeRandomCard(): Card = {
    val randIndex = Random.nextInt(arrayBuffer.size)
    val result = arrayBuffer(randIndex)

    /* deletion of last element of ArrayBuffer is constant time */
    arrayBuffer(randIndex) = arrayBuffer(arrayBuffer.size - 1)
    arrayBuffer.remove(arrayBuffer.size - 1)

    result
  }

  def returnCard(card: Card): Unit = {
    arrayBuffer.append(card)
  }

  def toVector: Vector[Card] = {
    arrayBuffer.toVector
  }

  def length: Int = arrayBuffer.length

  override def clone: CourtDeck = {
    new CourtDeck(arrayBuffer.clone)
  }
}
