package maze

sealed trait MazeInfo {
  val name: String
}

sealed trait RichMazeInfo[G <: PlainGrid[Position2D], M <: Maze[Cell2D]] extends MazeInfo {
  val grid: G
  val maze: M
}

case class RectMazeInfo(override val grid: RectGrid,
                        override val maze: Maze[Cell2DRect]) extends RichMazeInfo[RectGrid, Maze[Cell2DRect]] {
  override val name: String = "RectMaze"
}
