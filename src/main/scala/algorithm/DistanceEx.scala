package algorithm

import grid.{CellEx, GraphEx}

import scala.collection.mutable

//case class Position(val i: Int, val j: Int) { }

trait DistanceEx {
  def root: CellEx
  def graph: GraphEx
  def max: (CellEx, Int)
  def apply(cell: CellEx): Int
  def contains(cell: CellEx): Boolean
  def pathTo(position: CellEx): Option[List[CellEx]]
}

private class DistanceExImpl(val graph: GraphEx, val root: CellEx, val max: (CellEx, Int),
                             val distMap: Map[CellEx, Int]) extends DistanceEx {
  override def apply(cell: CellEx): Int = distMap(cell)

  override def pathTo(position: CellEx): Option[List[CellEx]] = {
    var path = List[CellEx]()
    val que = mutable.Queue(position)
    while (que.nonEmpty) {
      val current = que.dequeue()
      path = current +: path
      if (current != root) {
        val dist = this(current)
        val candidates = graph.linkedCells(current) match {
          case Some(cells) => cells.filter((p)=>this(p) == (dist-1)).toList
          case None => List[CellEx]()
        }
        if (candidates.nonEmpty) que.enqueue(candidates.head)
      } else {
        que.clear()
      }
    }
    if (path.head == root) Some(path) else None
  }

  override def contains(cell: CellEx): Boolean = distMap.contains(cell)
}

object DistanceEx {
  def from(graph: GraphEx, root: CellEx): Option[DistanceEx] = {
    var distMap = Map(root -> 0)
    val que = mutable.Queue(root)
    var maxItem = (root, 0)
    while (que.nonEmpty) {
      val size = que.size
      for (_ <- 0 until size) {
        val cell = que.dequeue()
        val curDist = distMap(cell)
        if (curDist > maxItem._2) {
          maxItem = (cell, curDist)
        }
        graph.linkedCells(cell) match {
          case Some(cells) =>
            cells.foreach((c: CellEx) => {
              if (!distMap.contains(c)) {
                distMap = distMap + (c -> (curDist+1))
                que.enqueue(c)
              }
            })
          case _ =>
        }
      }
    }
    if (distMap.nonEmpty) Some(new DistanceExImpl(graph, root, maxItem, distMap))
    else None
  }
}
