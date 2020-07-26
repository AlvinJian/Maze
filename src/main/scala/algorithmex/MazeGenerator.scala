package algorithmex

import maze.{Cell2D, Maze}

import scala.util.Random

trait MazeGenerator {
  type M <: Maze[Cell2D]
  def generate(random: Random, maze: M): M
}
