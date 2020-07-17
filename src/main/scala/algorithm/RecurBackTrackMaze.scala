package algorithm

import grid.{Cell2D, CellContainer, Graph, GraphEx, RectGrid}

import scala.collection.mutable
import scala.util.Random

object RecurBackTrackMaze extends MazeGenerator {
  override type T = CellContainer[Cell2D]

  override def generate(rand: Random, grid: T): Graph = {
    var graph = Graph(grid)
    var current: Option[Cell2D] = Some(grid.randomCell(rand))
    val stack = mutable.Stack[Cell2D]()
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
        var retreat: Option[Cell2D] = None
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
