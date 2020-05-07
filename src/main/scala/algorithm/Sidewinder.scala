package algorithm

import grid.Grid

import scala.collection.immutable.VectorBuilder
import scala.util.Random


class Sidewinder(val _r: Random) extends MazeSolver {
//  private var _r = new Random()

  override def solve(grid: Grid): Vector[Vector[CellLinkReader]] = {
    val graph: Vector[Vector[CellLink]] = CellLink.createFrom(grid)
    for (r <- 0 until grid.row) {
      var run = new VectorBuilder[CellLink]
      for (c <- 0 until grid.col) {
        run += graph(r)(c)
        val isEastEnd = grid(r,c).east match {
          case Some(_) => false
          case None => true
        }
        val isNorthEnd = grid(r,c).north match {
          case Some(_) => false
          case None => true
        }
        val shouldClose = if (isEastEnd || (!isNorthEnd && _r.nextInt(2) == 1)) true else false
        if (shouldClose) {
          val vec = run.result(); run.clear()
          val toNorth = vec(_r.nextInt(vec.size))
          toNorth.cell.north match {
            case Some(ncell) => toNorth.link(graph(ncell.row)(ncell.col))
            case None => {  }
          }
        } else {
          graph(r)(c).cell.east match {
            case Some(ecell) => graph(r)(c).link(graph(ecell.row)(ecell.col))
            case None => {  }
          }
        }
      }
    }

    graph
  }
}
