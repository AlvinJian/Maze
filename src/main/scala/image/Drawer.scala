package image

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2D, Maze, PolarMazeInfo, Position2D, RectMazeInfo}

trait Drawer {
  type M <: Maze[Cell2D]
  def maze: M
  def cellSize: Int
  def baseImage: ImmutableImage
  def finalImage(f: Position2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val g2 = new RichGraphics2D(mutableImage.awt().createGraphics())
    drawCells(g2, f)
    drawWalls(g2)
    mutableImage.toImmutableImage
  }
  protected def drawWalls(g2: RichGraphics2D): Unit
  protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit
}

object Drawer {
  def apply(maze: Maze[Cell2D], cellSize: Int): Drawer = maze.info match {
    case RectMazeInfo(grid, maze) => new RectMazeDrawer(grid, maze, cellSize)
    case PolarMazeInfo(grid, maze) => new PolarMazeDrawer(grid, maze, cellSize)
    case _ => ???
  }
}
