package image
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DRect, Maze, Position2D, RectMazeInfo}

private[image] class RectMazeDrawer(val info: RectMazeInfo, cSize: Int) extends Drawer {
  override type M = Maze[Cell2DRect]

  override def maze: M = info.maze

  val imgWidth: Int = info.grid.cols * cellSize
  val imgHeight: Int = info.grid.rows * cellSize

  override def baseImage: ImmutableImage =
    ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE, BufferedImage.TYPE_INT_RGB)

  override def drawWalls(prevImage: ImmutableImage): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    for (cell <- maze) {
      val (x1, y1, x2, y2) = calcRectCellPositions(cell)

      if (cell.north.isEmpty) {
        wallGraphics.drawLine(x1, y1, x2, y1)
      }
      if (cell.west.isEmpty) {
        wallGraphics.drawLine(x1, y1, x1, y2)
      }
      val linkedPos = maze.linked(cell.pos)
      val shouldDrawEast = cell.east match {
        case Some(eastCell) =>
          !linkedPos.contains(eastCell.pos)
        case _ => true
      }
      if (shouldDrawEast) wallGraphics.drawLine(x2, y1, x2, y2)
      val shouldDrawSouth = cell.south match {
        case Some(southCell) => !linkedPos.contains(southCell.pos)
        case _ => true
      }
      if (shouldDrawSouth) wallGraphics.drawLine(x1, y2, x2, y2)
    }
    mutableImage.toImmutableImage
  }

  override def drawCells(prevImage: ImmutableImage, f: Position2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- maze) {
      drawCell(cell, cellGraphics, f)
    }
    mutableImage.toImmutableImage
  }

  override def cellSize: Int = cSize

  protected def calcRectCellPositions(cell: Cell2DRect): (Int, Int, Int, Int) = {
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
