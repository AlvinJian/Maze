package utils
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DTriangle, CellContainer, Graph, GraphEx}

class TriangleMazeImageCreator(override val graph: Graph,
                               override val cellSize: Int) extends MazeImageCreator {
  private val halfWidth: Double = cellSize.toDouble / 2.0
  private val height: Double = cellSize.toDouble * scala.math.sqrt(3) / 2.0
  private val halfHeight: Double = height / 2.0
  private val triangleGrid = graph.grid.asInstanceOf[CellContainer[Cell2DTriangle]]

  override def baseImage: ImmutableImage = {
    val imgWidth: Int = cellSize * (triangleGrid.cols+1) / 2
    val imgHeight: Int = (height * triangleGrid.rows.toDouble).toInt
    ImmutableImage.filled(imgWidth+1, imgHeight+1,
      java.awt.Color.WHITE, BufferedImage.TYPE_INT_RGB)
  }

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)
    for (cell <- triangleGrid) {
      val (westX, midX, eastX, apexY, baseY) = calcVertexPositions(cell)
      if (cell.west.isEmpty) {
        wallGraphics.drawLine(westX, baseY, midX, apexY)
      }
      if (cell.east.isEmpty || !graph.isLinked(cell, cell.east.get)) {
        wallGraphics.drawLine(eastX, baseY, midX, apexY)
      }
      val shouldDrawSouthOrNorth = if (cell.isUpright) {
        cell.south.isEmpty
      } else {
        cell.north match {
          case Some(ncell) => !graph.isLinked(cell, ncell)
          case None => true
        }
      }
      if (shouldDrawSouthOrNorth) {
        wallGraphics.drawLine(eastX, baseY, westX, baseY)
      }
    }
    mutableImage.toImmutableImage
  }

  override def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- triangleGrid) {
      val (westX, midX, eastX, apexY, baseY) = calcVertexPositions(cell)
      cellGraphics.setColor(f(cell))
      val xpoints = Array(westX, midX, eastX)
      val ypoints = Array(baseY, apexY, baseY)
      cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
    }
    mutableImage.toImmutableImage
  }

  private def calcVertexPositions(cell: Cell2DTriangle): (Int, Int, Int, Int, Int) = {
    val cx: Double = halfWidth + cell.col.toDouble * halfWidth
    val cy = halfHeight + cell.row.toDouble * height
    val westX: Double = cx - halfWidth
    val midX: Double = cx
    val eastX: Double = cx + halfWidth
    val apexY = {
      if (cell.isUpright) cy - halfHeight
      else cy + halfHeight
    }
    val baseY = {
      if (cell.isUpright) cy + halfHeight
      else cy - halfHeight
    }
    (westX.toInt, midX.toInt, eastX.toInt, apexY.toInt, baseY.toInt)
  }
}
