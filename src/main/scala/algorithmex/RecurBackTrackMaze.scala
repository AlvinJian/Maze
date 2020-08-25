package algorithmex
import maze.{Cell2D, Maze, Position2D}

import scala.annotation.tailrec
import scala.util.Random

object RecurBackTrackMaze extends MazeGenerator {
  override type C = Cell2D

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    @tailrec
    def loop(pos2D: Position2D, prev: List[Position2D], curMaze: Maze[CC]): Maze[CC] = {
      val neighbors = curMaze.neighborsAt(pos2D).filter(c=>curMaze.linkedBy(c.pos).isEmpty)
      if (neighbors.nonEmpty) {
        val other = neighbors(random.nextInt(neighbors.size)).pos
        val newMaze = curMaze.link(pos2D, other) match {
          case Some(value) => value
          case None => curMaze
        }
        loop(other, pos2D +: prev, newMaze)
      } else {
        var stack = prev
        var optRetreat: Option[Position2D] = None
        while (stack.nonEmpty && optRetreat.isEmpty) {
          optRetreat = stack match {
            case ::(head, remaining) => {
              stack = remaining
              val neighbor1 = curMaze.neighborsAt(head)
              if (neighbor1.exists(c=>curMaze.linkedBy(c.pos).isEmpty)) {
                Some(head)
              } else None
            }
            case Nil => None
          }
        }
        optRetreat match {
          case Some(nextPos) => loop(nextPos, stack, curMaze)
          case None => curMaze
        }
      }
    }
    loop(maze.randomCell(random).pos, List[Position2D](), maze)
  }
}
