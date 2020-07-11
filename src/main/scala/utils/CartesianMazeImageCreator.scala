package utils
import java.awt
import java.awt.Color
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.canvas.drawables.{FilledRect, Line}
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DCart, CellContainer, Graph, GraphEx, GridEx, MaskedGrid}

class CartesianMazeImageCreator(override val graph: Graph,
                                override val cellSize: Int) extends MazeImageCreator {
  val imgWidth: Int = graph.grid.cols * cellSize
  val imgHeight: Int = graph.grid.rows * cellSize
  val grid = graph.grid.asInstanceOf[CellContainer[Cell2DCart]]

  override def baseImage: ImmutableImage = graph.grid match {
    case GridEx(_, _) => blankImage
    case maskedGrid: MaskedGrid => {
      val imgWidth = maskedGrid.cols * cellSize
      val imgHeight = maskedGrid.rows * cellSize
      val mutableImage = new MutableImage(blankImage.awt())
      val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
      // need to draw invalid cells
      for {
        r <- 0 until maskedGrid.rows
        c <- 0 until maskedGrid.cols
      } {
        val cell = maskedGrid(r,c)
        val (x1: Int, y1: Int, _, _) = calcRectCellPosition(cell)
        if (!maskedGrid.isValid(cell)) cellGraphics.setColor(Color.BLACK)
        else cellGraphics.setColor(Color.WHITE)
        new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
      }
      mutableImage.toImmutableImage
    }
    case _ => throw new java.lang.UnsupportedOperationException()
  }

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    for (cell <- grid) {
      val (x1, y1, x2, y2) = calcRectCellPosition(cell)

      if (cell.north.isEmpty) {
        new Line(x1, y1, x2, y1).draw(wallGraphics)
      }
      if (cell.west.isEmpty) {
        new Line(x1, y1, x1, y2).draw(wallGraphics)
      }
      val shouldDrawEast = cell.east match {
        case Some(ecell) => !graph.isLinked(cell, ecell)
        case _ => true
      }
      if (shouldDrawEast) new Line(x2, y1, x2, y2).draw(wallGraphics)
      val shouldDrawSouth = cell.south match {
        case Some(scell) => !graph.isLinked(cell, scell)
        case _ => true
      }
      if (shouldDrawSouth) new Line(x1, y2, x2, y2).draw(wallGraphics)
    }
    mutableImage.toImmutableImage
  }

  override def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- graph.grid) {
      val x1 = cell.col * cellSize
      val y1 = cell.row * cellSize
      cellGraphics.setColor(f(cell))
      new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
    }
    mutableImage.toImmutableImage
  }

  def calcRectCellPosition(cell2DCart: Cell2DCart): (Int, Int, Int, Int) = {
    val row = cell2DCart.row
    val col = cell2DCart.col
    val x1 = col * cellSize
    val y1 = row * cellSize
    val x2 = (col+1) * cellSize
    val y2 = (row+1) * cellSize
    (x1, y1, x2, y2)
  }

  def blankImage: ImmutableImage =
    ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE, BufferedImage.TYPE_INT_RGB)
}
