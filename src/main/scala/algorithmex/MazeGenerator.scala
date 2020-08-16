package algorithmex

import maze.{Cell2D, Maze}

import scala.util.Random

trait MazeGenerator {
  type C <: Cell2D
  def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC]
}
