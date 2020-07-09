package algorithm

import com.sksamuel.scrimage.color.RGBColor
import grid.{Cell2D, Graph}

import scala.collection.mutable

trait DistanceEx {
  def root: Cell2D
  def graph: Graph
  def max: (Cell2D, Int)
  def apply(cell: Cell2D): Int
  def contains(cell: Cell2D): Boolean
  def pathTo(position: Cell2D): List[Cell2D]
  def colorMapper(cell: Cell2D): RGBColor
}

private class DistanceExImpl(val graph: Graph, val root: Cell2D, val max: (Cell2D, Int),
                             val distMap: Map[Cell2D, Int]) extends DistanceEx {
  override def apply(cell: Cell2D): Int = distMap(cell)

  override def pathTo(position: Cell2D): List[Cell2D] = {
    var path = List[Cell2D]()
    val que = mutable.Queue(position)
    while (que.nonEmpty) {
      val current = que.dequeue()
      path = current +: path
      if (current != root) {
        val dist = this(current)
        val candidates = graph.linkedCells(current).filter((p)=>this(p) == (dist-1)).toList
        if (candidates.nonEmpty) que.enqueue(candidates.head)
      } else {
        que.clear()
      }
    }
    if (path.head == root) path else Nil
  }

  override def contains(cell: Cell2D): Boolean = distMap.contains(cell)

  def colorMapper(cell: Cell2D): RGBColor = {
    if (contains(cell)) {
//      val ratio: Double = (maxDist - dist) / maxDist
//      val dark: Int = (255.0 * ratio).round.toInt
//      val bright: Int = 128 + (127 * ratio).round.toInt
//      new RGBColor(dark, bright, dark)
      val dist = this(cell).toDouble
      val maxDist = max._2.toDouble
      val ratioRed = (maxDist-dist)/maxDist.toDouble
      val ratioGreen = dist.toDouble/maxDist.toDouble
      val r = (255.0 * ratioRed).toInt
      val g = (255.0 * ratioGreen).toInt
      val b = {
        if (dist == 0 || dist == maxDist) 255
        else (r + g)/2
      }
      new RGBColor(r, g, b)
    } else RGBColor.fromAwt(java.awt.Color.BLACK)
  }
}

object DistanceEx {
  def from(graph: Graph, root: Cell2D): Option[DistanceEx] = {
    if (!graph.grid.isValid(root)) return None
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
        graph.linkedCells(cell).foreach((c: Cell2D) => {
          if (!distMap.contains(c)) {
            distMap = distMap + (c -> (curDist+1))
            que.enqueue(c)
          }
        })
      }
    }
    if (distMap.nonEmpty) Some(new DistanceExImpl(graph, root, maxItem, distMap))
    else None
  }

  def createMax(graph: Graph, thru: Cell2D): Option[DistanceEx] = {
    var distMap = DistanceEx.from(graph, thru) match {
      case Some(value) => value
      case None => return None
    }
    val (newStart, _) = distMap.max
    DistanceEx.from(graph, newStart)
  }
}
