package algorithm

import grid.{CellEx, GraphEx, GridContainer, GridEx}

import scala.collection.mutable
import scala.util.Random

class HuntAndKillMaze(val rand: Random) extends MazeGenerator {
  override def generate(grid: GridContainer[CellEx]): GraphEx = {
    var graph = new GraphEx(grid)
    var cell: Option[CellEx] = Some(grid.randomCell(rand))
    while (cell.isDefined) {
      val ns = cell.get.neighbors
      if (ns.exists((c)=>graph.linkedCells(c).isEmpty)) {
        val next: CellEx = {
          val candidates = ns.filter((c)=>graph.linkedCells(c).isEmpty)
          candidates(rand.nextInt(candidates.size))
        }
        graph = graph.link(cell.get, next) match {
          case Some(value) => value
          case None => graph
        }
        cell = Some(next)
      } else {
        val ret = grid.find((c) => {
          if (graph.linkedCells(c).isEmpty &&
            c.neighbors.exists((nc)=>graph.linkedCells(nc).isDefined)) true
          else false
        })
        if (ret.isDefined) {
          cell = Some(ret.get)
          val candidates = cell.get.neighbors.filter((nc)=>graph.linkedCells(nc).isDefined)
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
