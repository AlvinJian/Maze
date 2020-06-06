package algorithm

import grid.{Cell2D, GraphEx, GridContainer, GridEx}

import scala.util.Random

object AldousBroderMaze extends MazeGenerator {
  override type T = GridContainer[Cell2D]

  override def generate(r: Random, grid: T): GraphEx = {
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
