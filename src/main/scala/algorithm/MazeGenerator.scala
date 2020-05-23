package algorithm

import grid.{CellEx, GraphEx, GridContainer}

trait MazeGenerator {
  def generate(grid: GridContainer[CellEx]): GraphEx
}
