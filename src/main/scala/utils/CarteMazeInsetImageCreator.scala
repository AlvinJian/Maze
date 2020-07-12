package utils
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DCart, Graph}

class CarteMazeInsetImageCreator(g: Graph,
                                 cs: Int,
                                 val inSet: Int) extends CartesianMazeImageCreator(g, cs) {
  def calcInsetPosition(cell2DCart: Cell2DCart):
    (Int, Int, Int, Int, Int, Int, Int, Int) = {
    val (_x1, _y1, _x2, _y2) = calcRectCellPosition(cell2DCart)
    val x1 = _x1
    val x2 = x1 + inSet
    val x3 = _x2 - inSet
    val x4 = _x2

    val y1 = _y1
    val y2 = y1 + inSet
    val y3 = _y2 - inSet
    val y4 = _y2
    (x1, x2, x3, x4, y1, y2, y3, y4)
  }

  override def baseImage: ImmutableImage = super.blankImage

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    if (inSet == 0) super.drawMazeWalls(prevImage)
    else {
      val mutableImage = new MutableImage(prevImage.awt())
      val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
      val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
      wallGraphics.setColor(wallColor)
      for (cell <- grid) {
        drawWalls(cell, wallGraphics)
      }
      mutableImage.toImmutableImage
    }
  }

  protected def drawWalls(cell: Cell2DCart, wallGraphics: RichGraphics2D): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)

    if (cell.north.isDefined && graph.isLinked(cell, cell.north.get)) {
      wallGraphics.drawLine(x2, y1, x2, y2)
      wallGraphics.drawLine(x3, y1, x3, y2)
    } else {
      wallGraphics.drawLine(x2, y2, x3, y2)
    }

    if (cell.south.isDefined && graph.isLinked(cell, cell.south.get)) {
      wallGraphics.drawLine(x2, y3, x2, y4)
      wallGraphics.drawLine(x3, y3, x3, y4)
    } else {
      wallGraphics.drawLine(x2, y3, x3, y3)
    }

    if (cell.west.isDefined && graph.isLinked(cell, cell.west.get)) {
      wallGraphics.drawLine(x1, y2, x2, y2)
      wallGraphics.drawLine(x1, y3, x2, y3)
    } else {
      wallGraphics.drawLine(x2, y2, x2, y3)
    }

    if (cell.east.isDefined && graph.isLinked(cell, cell.east.get)) {
      wallGraphics.drawLine(x3, y2, x4, y2)
      wallGraphics.drawLine(x3, y3, x4, y3)
    } else {
      wallGraphics.drawLine(x3, y2, x3, y3)
    }
  }

  override def drawCell(cell: Cell2DCart, cellGraphics: RichGraphics2D, f: Cell2D => RGBColor): Unit = {
    val (x1, x2, x3, x4, y1, y2, y3, y4) = calcInsetPosition(cell)
    cellGraphics.setColor(f(cell))
    var xleft = x2
    var xright = x3
    var ytop = y2
    var ybottom = y3
    if (cell.east.isDefined && graph.isLinked(cell, cell.east.get)) {
      xright = x4
    }
    if (cell.west.isDefined && graph.isLinked(cell, cell.west.get)) {
      xleft = x1
    }
    cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
    xleft = x2
    xright = x3
    if (cell.north.isDefined && graph.isLinked(cell, cell.north.get)) {
      ytop = y1
    }
    if (cell.south.isDefined && graph.isLinked(cell, cell.south.get)) {
      ybottom = y4
    }
    cellGraphics.fillRect(xleft, ytop, xright-xleft, ybottom-ytop)
  }
}
