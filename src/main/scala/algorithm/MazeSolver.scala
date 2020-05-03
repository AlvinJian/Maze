package algorithm

import grid.Grid

trait MazeSolver {
  def solve(grid: Grid): Vector[Vector[CellLink]]
}
