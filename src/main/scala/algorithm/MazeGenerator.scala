package algorithm

import grid.{Cell2D, CellContainer, Graph}

import scala.util.Random

trait MazeGenerator {
  type T <: CellContainer[Cell2D]
  def generate(r: Random, grid: T): Graph
}
