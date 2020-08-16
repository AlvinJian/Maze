package algorithmex

import maze.{Cell2D, Maze, Position2D}

import scala.annotation.tailrec
import scala.util.Random

object WilsonMaze extends MazeGenerator {
  override type C = Cell2D

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    def getUnvisitedOne(unvisited: Set[Position2D]): Position2D = {
      val arr = unvisited.toArray
      arr(random.nextInt(arr.length))
    }
    @tailrec
    def loop(maze: Maze[CC], unvisited: Set[Position2D]): Maze[CC] = {
      if (unvisited.nonEmpty) {
        val pos = getUnvisitedOne(unvisited)
        val path = List(pos)
        val pathSet = path.toSet
        @tailrec
        def roaming(pos: Position2D, prev: List[Position2D],
                    prevSet: Set[Position2D]): List[Position2D] = {
          val neighbors = maze.neighborsAt(pos)
          if (unvisited.contains(pos) && neighbors.nonEmpty) {
            val p = neighbors(random.nextInt(neighbors.size)).pos
            var set = prevSet; var path = prev
            while (set.contains(p)) {
              set = path match {
                case ::(head, next) => {
                  path = next
                  set - head
                }
                case Nil => set
              }
            }
            roaming(p, p +: path, set + p)
          } else prev
        }
        val ret = roaming(pos, path, pathSet)
        var newMaze = maze
        var newUnvisited = unvisited
        val _ = ret.foldLeft[Option[Position2D]](None) {
          (prev, cur)=>{
            if (prev.isDefined) {
              newMaze = newMaze.link(prev.get, cur).get
              newUnvisited = newUnvisited.filter(p=> p != prev.get && p != cur)
            }
            Some(cur)
          }
        }
        loop(newMaze, newUnvisited)
      } else maze
    }
    val unvisited = maze.map(c=>c.pos).toSet
    val seed = getUnvisitedOne(unvisited)
    loop(maze, unvisited - seed)
  }
}
