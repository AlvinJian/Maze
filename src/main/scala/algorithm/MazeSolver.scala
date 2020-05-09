package algorithm

import grid.Grid

@deprecated
trait MazeSolver {
  def solve(grid: Grid): Vector[Vector[CellLinkReader]]
}
