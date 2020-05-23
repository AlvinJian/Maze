package algorithm
import grid.{CellEx, GraphEx, GridContainer, GridEx}

import scala.collection.immutable.VectorBuilder
import scala.util.Random

class SidewinderMaze(val _r: Random) extends MazeGenerator {
  override def generate(grid: GridContainer[CellEx]): GraphEx = {
    var graph: GraphEx = new GraphEx(grid)
    for (r <- 0 until grid.rows) {
      var run = Vector[CellEx]()
      for (c <- 0 until grid.cols) {
        val cell = grid(r, c)
        run = run :+ cell
        val isEastEnd = grid(r,c).east.isEmpty
        val isNorthEnd = grid(r,c).north.isEmpty
        val shouldClose =
          if (isEastEnd || (!isNorthEnd && _r.nextInt(2) == 1)) true
          else false
        if (shouldClose) {
          val toNorth = run(_r.nextInt(run.size))
          if (toNorth.north.isDefined) {
            graph = graph.link(toNorth, toNorth.north.get) match {
              case Some(g) => g
              case None => graph
            }
          }
          run = Vector[CellEx]()
        } else {
          if (cell.east.isDefined) {
            graph = graph.link(cell, cell.east.get) match {
              case Some(g) => g
              case None => graph
            }
          }
        }
      }
    }

    graph
  }
}
