package maze

sealed trait MazeInfo {
  type G <: PlainGrid[Position2D]
  type M <: Maze[Cell2D]
  val name: String
  val grid: G
  val maze: M
}

case class RectMazeInfo(override val grid: RectGrid,
                        override val maze: Maze[Cell2DRect]) extends MazeInfo {
  override type G = RectGrid
  override type M = Maze[Cell2DRect]
  override val name: String = "RectMaze"
}

case class PolarMazeInfo(override val grid: PolarGrid,
                         override val maze: Maze[Cell2DPolar]) extends MazeInfo {
  override type G = PolarGrid
  override type M = Maze[Cell2DPolar]
  override val name: String = "PolarMaze"
}
