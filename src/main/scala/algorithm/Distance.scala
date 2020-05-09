package algorithm

import scala.collection.immutable
import scala.collection.mutable.Map
import scala.collection.mutable.Queue

@deprecated
class Distance(val root: CellLinkReader) {
  private val distMap = Map[CellLinkReader, Int]()
  build()

  def get(link: CellLinkReader): Option[Int] = distMap.get(link)
  def map: immutable.Map[CellLinkReader, Int] = distMap.toMap

  private def build(): Unit = {
    var dist = 0
    val que = Queue[CellLinkReader]()
    que += root
    while (que.nonEmpty) {
      val size = que.size
      for (_ <- 0 until size) {
        val link = que.dequeue()
        if (!distMap.contains(link)) {
          distMap += (link -> dist)
          que ++= link.linked
        }
      }
      dist += 1
    }
  }
}
