package image

import algorithmex.DistanceMap
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2D, HexMazeInfo, Maze, PolarMazeInfo, Position2D, RectMazeInfo, TriangleMazeInfo, WeaveMazeInfo}

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
    case TriangleMazeInfo(grid, maze) => new TriangleMazeDrawer(grid, maze, cellSize)
    case HexMazeInfo(grid, maze) => new HexMazeDrawer(grid, maze, cellSize)
    case WeaveMazeInfo(grid, maze) => new WeaveMazeDrawer(grid, maze, cellSize, cellSize/6)
    case _ => ???
  }

  def distToColor(distMap: DistanceMap, cell: Cell2D): RGBColor = {
    if (distMap.data.contains(cell)) {
      //      val ratio: Double = (maxDist - dist) / maxDist
      //      val dark: Int = (255.0 * ratio).round.toInt
      //      val bright: Int = 128 + (127 * ratio).round.toInt
      //      new RGBColor(dark, bright, dark)
      val dist = distMap.data(cell).toDouble
      val maxDist = distMap.max._2.toDouble
      val ratioRed = (maxDist-dist)/maxDist.toDouble
      val ratioGreen = dist.toDouble/maxDist.toDouble
      val r = (255.0 * ratioRed).toInt
      val g = (255.0 * ratioGreen).toInt
      val b = {
        if (dist == 0 || dist == maxDist) 255
        else (r + g)/2
      }
      new RGBColor(r, g, b)
    } else RGBColor.fromAwt(java.awt.Color.BLACK)
  }
}
