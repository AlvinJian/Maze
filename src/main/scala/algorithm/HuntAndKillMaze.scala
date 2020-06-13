package algorithm

import grid.{Cell2D, GraphEx, CellContainer, GridEx}

import scala.util.Random

object HuntAndKillMaze extends MazeGenerator {
  override type T = CellContainer[Cell2D]

  override def generate(rand: Random, grid: T): GraphEx = {
    var graph = new GraphEx(grid)
    var cell: Option[Cell2D] = Some(grid.randomCell(rand))
    while (cell.isDefined) {
      val ns = cell.get.neighbors
      if (ns.exists((c)=>graph.linkedCells(c).isEmpty)) {
        val next: Cell2D = {
          val candidates = ns.filter((c)=>graph.linkedCells(c).isEmpty)
          candidates(rand.nextInt(candidates.size))
        }
        graph = graph.link(cell.get, next) match {
          case Some(value) => value
          case None => graph
        }
        cell = Some(next)
      } else {
        val ret = grid.find(c => graph.linkedCells(c).isEmpty &&
            c.neighbors.exists(nc => graph.linkedCells(nc).nonEmpty)
        )
        if (ret.isDefined) {
          cell = Some(ret.get)
          val candidates = cell.get.neighbors.filter((nc)=>graph.linkedCells(nc).nonEmpty)
          val other = candidates(rand.nextInt(candidates.size))
          graph = graph.link(cell.get, other) match {
            case Some(value) => value
            case None => graph
          }
        } else {
          cell = None
        }
      }
    }
    graph
  }
}
