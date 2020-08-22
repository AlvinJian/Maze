package image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import maze.{Cell2D, Maze, Position2D, RectMazeInfo}

trait Drawer {
  type M <: Maze[Cell2D]
  def maze: M
  def cellSize: Int
  def baseImage: ImmutableImage
  def drawWalls(prevImage: ImmutableImage): ImmutableImage
  def drawCells(prevImage: ImmutableImage, f: Position2D => RGBColor): ImmutableImage
}

object Drawer {
  def apply(maze: Maze[Cell2D], cellSize: Int): Drawer = maze.info match {
    case mazeInfo: RectMazeInfo => new RectMazeDrawer(mazeInfo.grid, mazeInfo.maze, cellSize)
    case _ => ???
  }
}
