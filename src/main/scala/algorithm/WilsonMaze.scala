package algorithm

import grid.{CellEx, GraphEx, GridContainer, GridEx}

import scala.collection.mutable
import scala.util.Random

class WilsonMaze(val rand: Random) extends MazeGenerator {
  override def generate(grid: GridContainer[CellEx]): GraphEx = {
    var graph = new GraphEx(grid)
    var unvisited = grid.toSet
    val getUnvisited: ()=>CellEx = () => {
      val arr = unvisited.toArray
      arr(rand.nextInt(arr.length))
    }
    val seed = getUnvisited()
    unvisited = unvisited - seed
    while (unvisited.nonEmpty) {
      val pathSet = mutable.Set[CellEx]()
      val path = mutable.ArrayDeque[CellEx]()
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
      for (i <- 0 until path.size-1) {
        graph = graph.link(path(i), path(i+1)) match {
          case Some(value) => {
            unvisited = unvisited.filter((c) => c != path(i) && c != path(i+1))
            value
          }
          case None => graph
        }
      }
    }
    graph
  }
}
