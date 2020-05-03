package algorithm

import grid.{Cell, Grid}

import scala.util.Random

class BinaryTree extends MazeSolver {
  private var _r = new Random()

  def solve(grid: Grid): Vector[Vector[CellLink]] = {
    val graph: Vector[Vector[CellLink]] = CellLink.createFrom(grid)
    for (c <- grid) {
      val curLink = graph(c.row)(c.col)
      val cands = List(c.north, c.east).flatten
      if (cands.nonEmpty) {
        val cand = if (cands.size > 1) cands(_r.nextInt(cands.size)) else cands(0)
        val otherLink = graph(cand.row)(cand.col)
        curLink.link(otherLink)
      }
    }
    graph
  }
}
