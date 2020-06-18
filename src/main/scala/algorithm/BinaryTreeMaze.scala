package algorithm
import grid.{Cell2DCart, GraphEx, CellContainer, GridEx}

import scala.util.Random

object BinaryTreeMaze extends MazeGenerator {
  override type T = CellContainer[Cell2DCart]

  override def generate(_r: Random, grid: T): GraphEx = {
    var graph = new GraphEx(grid)
    for (cell <- grid) {
      val candidates: List[Cell2DCart] = List(cell.north, cell.east).flatten
      if (candidates.nonEmpty) {
        val otherCell = candidates(_r.nextInt(candidates.size))
        graph = graph.link(cell, otherCell) match {
          case Some(newGraph) => newGraph
          case None => graph
        }
      }
    }

    graph
  }
}
