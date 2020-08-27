package image
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DRect, Maze, Position2D, RectGrid}

private[image] class RectMazeDrawer(val grid: RectGrid,
                                    val maze: Maze[Cell2DRect],
                                    cSize: Int) extends Drawer {
  override type M = Maze[Cell2DRect]

  val imgWidth: Int = grid.cols * cellSize
  val imgHeight: Int = grid.rows * cellSize

  override def baseImage: ImmutableImage =
    ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE, BufferedImage.TYPE_INT_RGB)

  override protected def drawWalls(g2: RichGraphics2D): Unit = {
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    val wallGraphics = g2
    wallGraphics.setColor(wallColor)

    for (cell <- maze) {
      val (x1, y1, x2, y2) = calcRectCellPositions(cell)

      if (cell.north.isEmpty) {
        wallGraphics.drawLine(x1, y1, x2, y1)
      }
      if (cell.west.isEmpty) {
        wallGraphics.drawLine(x1, y1, x1, y2)
      }
      val linkedCells = maze.linkedBy(cell.pos).toSet
      val shouldDrawEast = cell.east match {
        case Some(eastCell) => !linkedCells.contains(eastCell)
        case _ => true
      }
      if (shouldDrawEast) wallGraphics.drawLine(x2, y1, x2, y2)
      val shouldDrawSouth = cell.south match {
        case Some(southCell) => !linkedCells.contains(southCell)
        case _ => true
      }
      if (shouldDrawSouth) wallGraphics.drawLine(x1, y2, x2, y2)
    }
  }

  override protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val cellGraphics = g2
    for (cell <- maze) {
      drawCell(cell, cellGraphics, f)
    }
  }

  override def cellSize: Int = cSize

  def calcRectCellPositions(cell: Cell2DRect): (Int, Int, Int, Int) = {
    val row = cell.pos.row
    val col = cell.pos.col
    val x1 = col * cellSize
    val y1 = row * cellSize
    val x2 = (col+1) * cellSize
    val y2 = (row+1) * cellSize
    (x1, y1, x2, y2)
  }

  protected def drawCell(cell: Cell2DRect, cellGraphics: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val x1 = cell.pos.col * cellSize
    val y1 = cell.pos.row * cellSize
    cellGraphics.setColor(f(cell.pos))
    cellGraphics.fillRect(x1, y1, cellSize, cellSize)
  }
}
