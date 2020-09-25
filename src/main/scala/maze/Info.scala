package maze

sealed trait MazeInfo[+G <: PlainGrid[Position2D], +M <: Maze[Cell2D]] {
//  type G <: PlainGrid[Position2D]
//  type M <: Maze[Cell2D]
  val name: String
  val grid: G
  val maze: M
}

case class RectMazeInfo(override val grid: RectGrid,
                        override val maze: Maze[Cell2DRect]) extends MazeInfo[RectGrid, Maze[Cell2DRect]] {
//  override type G = RectGrid
//  override type M = Maze[Cell2DRect]
  override val name: String = "RectMaze"
}

case class PolarMazeInfo(override val grid: PolarGrid,
                         override val maze: Maze[Cell2DPolar]) extends MazeInfo[PolarGrid, Maze[Cell2DPolar]] {
//  override type G = PolarGrid
//  override type M = Maze[Cell2DPolar]
  override val name: String = "PolarMaze"
}

case class TriangleMazeInfo(override val grid: RectGrid,
                            override val maze: Maze[Cell2DTriangle]) extends MazeInfo[RectGrid, Maze[Cell2DTriangle]] {
  override val name: String = "TriangleMaze"
}

case class HexMazeInfo(override val grid: RectGrid,
                       override val maze: Maze[Cell2DHex]) extends MazeInfo[RectGrid, Maze[Cell2DHex]] {
  override val name: String = "HexMaze"
}

case class WeaveMazeInfo(override val grid: RectGrid,
                         override val maze: Maze[Cell2DWeave]) extends MazeInfo[RectGrid, Maze[Cell2DWeave]] {
  override val name: String = "WeaveMaze"
}
