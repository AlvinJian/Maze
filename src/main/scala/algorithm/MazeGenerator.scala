package algorithm

import grid.{Cell2D, GraphEx, GridContainer}

import scala.util.Random

trait MazeGenerator {
  type T <: GridContainer[Cell2D]
  def generate(r: Random, grid: T): GraphEx
}
