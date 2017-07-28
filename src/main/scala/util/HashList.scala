package util

import scala.collection.mutable

class HashList[T] {

  protected val backingMap: mutable.HashMap[T, Int] = new mutable.HashMap

  def add(elem: T): Unit = {
    if (backingMap.contains(elem))
      backingMap(elem) += 1
    else
      backingMap(elem) = 1
  }

  def remove(elem: T): Unit = {
    require(backingMap.contains(elem))
    if (backingMap(elem) == 1)
      backingMap.remove(elem)
    else
      backingMap(elem) -= 1
  }

  def contains(elem: T): Unit = {
    backingMap.contains(elem)
  }
}
