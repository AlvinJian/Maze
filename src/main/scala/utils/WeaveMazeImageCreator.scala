package utils

import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import grid.{Cell2DCart, Cell2DWeave, CellContainer, Graph}

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
          drawHiddenCell(cell, wallGraphics)
        } else {
          super.drawWalls(cell, wallGraphics)
        }
      }
      mutableImage.toImmutableImage
    }
  }

  protected def drawHiddenCell(cell: Cell2DWeave, wallGraphics: RichGraphics2D): Unit = {
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
}
