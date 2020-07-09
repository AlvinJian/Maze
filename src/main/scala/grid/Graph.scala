package grid

import scala.util.Random

trait Graph {
  val grid: CellContainer[Cell2D]

  def link(from: Cell2D, to: Cell2D): Option[Graph]

  def isLinked(c1: Cell2D, c2: Cell2D): Boolean

  def linkedCells(cell: Cell2D): Set[Cell2D]

  def linkedCells(r: Int, c: Int): Set[Cell2D] = linkedCells(grid(r, c))

  def deadEnds: List[Cell2D]
}

object Graph {
  def apply(grid: CellContainer[Cell2D]): Graph = grid match {
    case weaveGrid: WeaveGrid => weaveGrid.asInstanceOf[Graph]
    case _ => new GraphEx(grid)
  }

  def braid(rand: Random, graph: Graph, param: Double = 1.0): Graph = {
    val deadEnds: Array[Cell2D] = (rand shuffle graph.deadEnds).toArray
    @scala.annotation.tailrec
    def loop(maze: Graph, i: Int): Graph = {
      if (i == deadEnds.length) maze
      else if (maze.linkedCells(deadEnds(i)).size != 1 || rand.nextDouble() > param) {
        loop(maze, i+1)
      } else {
        val cell = deadEnds(i)
        val candidates = cell.neighbors.filter(nc => !maze.isLinked(cell, nc))
        val bestCandidates = candidates.filter(c => maze.linkedCells(c).size == 1)
        val best: Cell2D = {
          if (bestCandidates.nonEmpty) bestCandidates(rand.nextInt(bestCandidates.size))
          else candidates(rand.nextInt(candidates.size))
        }
        val _maze = maze.link(cell, best) match {
          case Some(value) => value
          case None => maze
        }
        loop(_maze, i+1)
      }
    }
    loop(graph, 0)
  }
}
