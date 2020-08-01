package algorithmex

import maze.{Cell2D, Maze}

import scala.util.Random

trait MazeGenerator {
  type M <: Maze[_]
  def generate(random: Random, maze: M): M
}
