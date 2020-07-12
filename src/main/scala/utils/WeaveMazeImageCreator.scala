package utils

import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import grid.{Cell2D, Cell2DCart, Cell2DHidden, Cell2DWeave, CellContainer, Graph}

class WeaveMazeImageCreator(g: Graph, cs: Int,
                            inset: Int) extends CarteMazeInsetImageCreator(g, cs, inset) {
  override val grid: CellContainer[Cell2DWeave] = g.grid.asInstanceOf[CellContainer[Cell2DWeave]]

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    if (inSet == 0) super.drawMazeWalls(prevImage)
    else {
      val mutableImage = new MutableImage(prevImage.awt())
      val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
      val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
      wallGraphics.setColor(wallColor)
      for (cell <- grid) {
        if (cell.isHidden) {
          drawHiddenCellWalls(cell, wallGraphics)
        } else {
          super.drawWalls(cell, wallGraphics)
        }
      }
      mutableImage.toImmutableImage
    }
  }

  protected def drawHiddenCellWalls(cell: Cell2DWeave, wallGraphics: RichGraphics2D): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    if (cell.isVerticalLinked) {
      wallGraphics.drawLine(x2, y1, x2, y2)
      wallGraphics.drawLine(x3, y1, x3, y2)
      wallGraphics.drawLine(x2, y3, x2, y4)
      wallGraphics.drawLine(x3, y3, x3, y4)
    } else {
      wallGraphics.drawLine(x1, y2, x2, y2)
      wallGraphics.drawLine(x1, y3, x2, y3)
      wallGraphics.drawLine(x3, y2, x4, y2)
      wallGraphics.drawLine(x3, y3, x4, y3)
    }
  }

  override def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    val g = graph.grid.asInstanceOf[CellContainer[Cell2DWeave]]
    for (cell <- g) {
      if (!cell.isHidden) {
        drawCell(cell, cellGraphics, f)
      } else {
        drawHiddenCell(cell, cellGraphics, f)
      }
    }
    mutableImage.toImmutableImage
  }

  protected def drawHiddenCell(cell: Cell2DWeave, cellGraphics: RichGraphics2D,
                               f: Cell2D => RGBColor): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    cellGraphics.setColor(f(cell))
    var xleft = x2
    var xright = x3
    var ytop = y2
    var ybottom = y3
    def drawRect(): Unit = cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
    if (cell.north.isDefined && graph.isLinked(cell, cell.north.get)) {
      ytop = y1; ybottom = y2
      xleft = x2; xright = x3
      drawRect()
    }
    if (cell.south.isDefined && graph.isLinked(cell, cell.south.get)) {
      ytop = y3; ybottom = y4
      xleft = x2; xright = x3
      drawRect()
    }
    if (cell.west.isDefined && graph.isLinked(cell, cell.west.get)) {
      ytop = y2; ybottom = y3
      xleft = x1; xright = x2
      drawRect()
    }
    if (cell.east.isDefined && graph.isLinked(cell, cell.east.get)) {
      ytop = y2; ybottom = y3
      xleft = x3; xright = x4
      drawRect()
    }
  }
}
