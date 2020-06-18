package algorithm
import grid.{Cell2DCart, GraphEx, CellContainer, GridEx}

import scala.collection.immutable.VectorBuilder
import scala.util.Random

object SidewinderMaze extends MazeGenerator {
  override type T = CellContainer[Cell2DCart]

  override def generate(_r: Random, grid: T): GraphEx = {
    var graph: GraphEx = new GraphEx(grid)
    for (r <- 0 until grid.rows) {
      var run = Vector[Cell2DCart]()
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
          run = Vector[Cell2DCart]()
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
