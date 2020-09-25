package algorithmex
import maze.{Cell2DRect, Maze}

import scala.util.Random

object BinaryTreeMaze extends MazeGenerator {
  override type C = Cell2DRect

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    val positions = maze.map(c => c.pos)
    positions.foldLeft(maze)((prevMaze, p) => {
      val candidates = List(prevMaze.at(p).flatMap(c => c.north),
        prevMaze.at(p).flatMap(c => c.east)).flatten
      if (candidates.nonEmpty) {
        val other = candidates(random.nextInt(candidates.size))
        prevMaze.link(p, other.pos) match {
          case Some(value) => value
          case None => prevMaze
        }
      } else prevMaze
    })
  }
}
