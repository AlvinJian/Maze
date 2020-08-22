package algorithmex

import maze.{Cell2D, Maze, MazeInfo, Position2D}

trait DistanceMap {
  def info: MazeInfo
  def apply(pos: Position2D): Int
  def root: Position2D
  def max: (Position2D, Int)
  def contains(pos: Position2D): Boolean
  def pathTo(pos: Position2D): List[Position2D]
}

object DistanceMap {
  def apply(start: Position2D, maze: Maze[Cell2D]): DistanceMap = ???
  def maximize(seed: Position2D, maze: Maze[Cell2D]): DistanceMap = ???
}
