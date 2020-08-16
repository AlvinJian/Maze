package algorithmex
import maze.{Cell2D, Maze, Position2D}

import scala.util.Random

object AldousBroderMaze extends MazeGenerator {
  override type C = Cell2D

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    val pos = maze.randomCell(random).pos
    @scala.annotation.tailrec
    def loop(unvisited: Int, pos: Position2D, maze: Maze[CC]): Maze[CC] = {
      if (unvisited > 0 && maze.at(pos).isDefined) {
        val cell = maze.at(pos).get
        val randIndex = random.nextInt(cell.neighbors.size)
        val neighborPos = cell.neighbors(randIndex).pos
        if (maze.linked(neighborPos).isEmpty) {
          maze.link(pos, neighborPos) match {
            case Some(newMaze) => loop(unvisited - 1, neighborPos, newMaze)
            case None => loop(unvisited, pos, maze)
          }
        } else loop(unvisited, neighborPos, maze)
      } else maze
    }
    loop(maze.size-1, pos, maze)
  }
}
