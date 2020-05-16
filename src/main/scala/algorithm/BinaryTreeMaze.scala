package algorithm
import grid.{CellEx, GraphEx, GridEx}

import scala.util.Random

class BinaryTreeMaze(val _r: Random) extends MazeGenerator {
  override def generate(grid: GridEx): GraphEx = {
    var graph = new GraphEx(grid)
    for (cell <- grid) {
      val candidates: List[CellEx] = List(cell.north, cell.east).flatten
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
