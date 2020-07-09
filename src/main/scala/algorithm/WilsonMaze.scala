package algorithm

import grid.{Cell2D, CellContainer, Graph, GraphEx}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object WilsonMaze extends MazeGenerator {
  override type T = CellContainer[Cell2D]

  override def generate(rand: Random, grid: T): Graph = {
    var graph = Graph(grid)
    var unvisited = grid.toSet
    val getUnvisited: ()=> Cell2D = () => {
      val arr = unvisited.toArray
      arr(rand.nextInt(arr.length))
    }
    val seed = getUnvisited()
    unvisited = unvisited - seed
    while (unvisited.nonEmpty) {
      val pathSet = mutable.Set[Cell2D]()
      val path = mutable.ArrayDeque[Cell2D]()
      var p = getUnvisited()
      pathSet.add(p); path.prepend(p)
      while (unvisited.contains(p)) {
        val ns = p.neighbors
        p = ns(rand.nextInt(ns.size))
        while (pathSet.contains(p)) {
          val last = path.removeHead()
          pathSet.remove(last)
        }
        pathSet.add(p); path.prepend(p)
      }
      @tailrec
      def linkPath(path: Seq[Cell2D], i: Int, graph: Graph): Graph = {
        if (i < path.size-1) {
          graph.link(path(i), path(i + 1)) match {
            case Some(value) => {
              unvisited = unvisited.filter((c) => c != path(i) && c != path(i+1))
              linkPath(path, i+1, value)
            }
            case None => graph
          }
        } else graph
      }
      graph = linkPath(path.toSeq, 0, graph)
    }
    graph
  }
}
