package algorithmex
import maze.{Cell2D, Maze, Position2D}

import scala.annotation.tailrec
import scala.util.Random

object HuntAndKillMaze extends MazeGenerator {
  override type C = Cell2D

  def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    @tailrec
    def loop(position: Position2D, maze: Maze[CC]): Maze[CC] = {
      val neighbors = maze.neighborsAt(position).filter(c=>maze.linkedBy(c.pos).isEmpty)
      if (neighbors.nonEmpty) {
        val next = {
          val id = random.nextInt(neighbors.size)
          neighbors(id).pos
        }
        val newMaze = maze.link(position, next) match {
          case Some(value) => value
          case None => maze
        }
        loop(next, newMaze)
      } else {
        maze.find(c => maze.linkedBy(c.pos).isEmpty &&
          c.neighbors.exists(nc=>maze.linkedBy(nc.pos).nonEmpty)) match {
          case Some(cell) => {
            val candidates = cell.neighbors.filter(nc=>maze.linkedBy(nc.pos).nonEmpty)
            val other = candidates(random.nextInt(candidates.size)).pos
            val newMaze = maze.link(cell.pos, other) match {
              case Some(value) => value
              case None => maze
            }
            loop(cell.pos, newMaze)
          }
          case None => maze
        }
      }
    }
    val start = maze.randomCell(random).pos
    loop(start, maze)
  }
}
