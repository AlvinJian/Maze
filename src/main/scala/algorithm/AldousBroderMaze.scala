package algorithm

import grid.{GraphEx, GridEx}

import scala.collection.mutable
import scala.util.Random

class AldousBroderMaze(val r: Random) extends MazeGenerator {
//  private val r: Random = new Random()

  override def generate(grid: GridEx): GraphEx = {
    var graph = new GraphEx(grid)
    var unvisited: Int = grid.size
    var cell = grid.randomCell(r)
    unvisited -= 1
    while (unvisited > 0) {
      val ns = cell.neighbors
      val neighbor = ns(r.nextInt(ns.size))
      if (graph.linkedCells(neighbor).isEmpty) {
        graph = graph.link(cell, neighbor) match {
          case Some(value) => {
            unvisited -= 1
            value
          }
          case None => graph
        }
      }
      cell = neighbor
    }
    graph
  }
}
