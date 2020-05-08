package algorithm

import grid.{GraphEx, GridEx}

trait MazeGenerator {
  def generate(grid: GridEx): GraphEx
}
