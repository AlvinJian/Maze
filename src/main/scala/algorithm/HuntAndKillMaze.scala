package algorithm

import grid.{CellEx, GraphEx, GridEx}

import scala.collection.mutable
import scala.util.Random

class HuntAndKillMaze(val rand: Random) extends MazeGenerator {
  override def generate(grid: GridEx): GraphEx = {
    var graph = new GraphEx(grid)
    var unvisited = grid.toSet
    val getUnvisited = () => {
      val arr = unvisited.toArray
      arr(rand.nextInt(arr.size))
    }
    var cell = getUnvisited()
    unvisited = unvisited - cell
    while (unvisited.nonEmpty) {
      val ns = cell.neighbors
      if (ns.nonEmpty && ns.exists((c)=>unvisited.contains(c))) {
        val next: CellEx = {
          val candidates = ns.filter((c)=>unvisited.contains(c))
          candidates(rand.nextInt(candidates.size))
        }
        graph = graph.link(cell, next) match {
          case Some(value) => value
          case None => graph
        }
        cell = next; unvisited = unvisited - cell
      } else {
        val ret = grid.find((c) => {
          if (unvisited.contains(c) &&
            c.neighbors.exists((c) => !unvisited.contains(c))) true
          else false
        })
        if (ret.isDefined) {
          cell = ret.get
          unvisited = unvisited - cell
          val targets = cell.neighbors.filter((c) => !unvisited.contains(c))
          if (targets.nonEmpty) {
            graph = graph.link(cell, targets(rand.nextInt(targets.size))) match {
              case Some(value) => value
              case None => graph
            }
          }
        }
      }
    }
    graph
  }
}
