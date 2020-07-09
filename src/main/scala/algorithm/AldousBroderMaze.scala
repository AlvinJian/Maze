package algorithm

import grid.{Cell2D, CellContainer, Graph, GraphEx, GridEx}

import scala.util.Random

object AldousBroderMaze extends MazeGenerator {
  override type T = CellContainer[Cell2D]

  override def generate(r: Random, grid: T): Graph = {
    val graph = Graph(grid)
    val cell = grid.randomCell(r)
    val unvisited: Int = grid.size-1
    @scala.annotation.tailrec
    def loop(unvisited: Int, cell: Cell2D, graph: Graph): Graph = {
      if (unvisited > 0) {
        val ns = cell.neighbors
        val neighbor = ns(r.nextInt(ns.size))
        if (graph.linkedCells(neighbor).isEmpty) {
          val ret = graph.link(cell, neighbor)
          if (ret.isDefined) {
            loop(unvisited-1, neighbor, ret.get)
          } else {
            loop(unvisited, cell, graph)
          }
        } else loop(unvisited, neighbor, graph)
      } else graph
    }
    loop(unvisited, cell, graph)
  }
}
