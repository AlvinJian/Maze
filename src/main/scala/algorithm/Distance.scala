package algorithm

import scala.collection.mutable.Map

class Distance(val root: CellLink) {
  private val distMap = Map[CellLink, Int]()

  def get(link: CellLink): Option[Int] = distMap.get(link)
  def add(link: CellLink, distance: Int): Unit = {
    distMap += (link -> distance)
  }

  def links: Set[CellLink] = distMap.keySet.toSet
}
