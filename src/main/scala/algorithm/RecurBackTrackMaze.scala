package algorithm

import grid.{CellEx, GraphEx, GridEx}

import scala.collection.mutable
import scala.util.Random

class RecurBackTrackMaze(rand: Random) extends MazeGenerator {
  override def generate(grid: GridEx): GraphEx = {
    var graph = new GraphEx(grid)
    var current: Option[CellEx] = Some(grid.randomCell(rand))
    val stack = mutable.Stack[CellEx]()
    while (current.isDefined) {
      val neighbors = current.get.neighbors.filter((c)=>graph.linkedCells(c).isEmpty)
      if (neighbors.nonEmpty) {
        val next = neighbors(rand.nextInt(neighbors.size))
        graph = graph.link(current.get, next) match {
          case Some(value) => value
          case None => graph
        }
        stack.push(current.get)
        current = Some(next)
      } else {
        var retreat: Option[CellEx] = None
        while (stack.nonEmpty && retreat.isEmpty) {
          val cell = stack.pop()
          if (cell.neighbors.exists((c)=>graph.linkedCells(c).isEmpty)) {
            retreat = Some(cell)
          }
        }
        current = retreat
      }
    }
    graph
  }
}
