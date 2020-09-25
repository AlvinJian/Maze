package image

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DTriangle, Maze, Position2D, RectGrid}

private[image] class TriangleMazeDrawer(val grid: RectGrid,
                                        val m: Maze[Cell2DTriangle],
                                        cSize: Int) extends Drawer {
  override type M = Maze[Cell2DTriangle]

  private val halfWidth: Double = cellSize.toDouble / 2.0
  private val height: Double = cellSize.toDouble * scala.math.sqrt(3) / 2.0
  private val halfHeight: Double = height / 2.0

  override def maze: M = m

  override def cellSize: Int = cSize

  override def baseImage: ImmutableImage = {
    val imgWidth: Int = cellSize * (grid.cols+1) / 2
    val imgHeight: Int = (height * grid.rows.toDouble).toInt
    ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE, BufferedImage.TYPE_INT_RGB)
  }

  override protected def drawWalls(g2: RichGraphics2D): Unit = {
    val wallGraphics = g2
    val wallColor = RGBColor.fromAwt(java.awt.Color.BLACK)
    wallGraphics.setColor(wallColor)
    for (cell <- maze) {
      val (westX, midX, eastX, apexY, baseY) = vertexPositions(cell)
      if (cell.west.isEmpty) {
        wallGraphics.drawLine(westX, baseY, midX, apexY)
      }
      if (cell.east.isEmpty || !cell.linked.contains(cell.east.get)) {
        wallGraphics.drawLine(eastX, baseY, midX, apexY)
      }
      val shouldDrawSouthOrNorth = if (cell.isUpright) {
        cell.south.isEmpty
      } else {
        cell.north match {
          case Some(northCell) => !cell.linked.contains(northCell)
          case None => true
        }
      }
      if (shouldDrawSouthOrNorth) {
        wallGraphics.drawLine(eastX, baseY, westX, baseY)
      }
    }
  }

  override protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val cellGraphics = g2
    for (cell <- maze) {
      val (westX, midX, eastX, apexY, baseY) = vertexPositions(cell)
      cellGraphics.setColor(f(cell.pos))
      val xpoints = Array(westX, midX, eastX)
      val ypoints = Array(baseY, apexY, baseY)
      cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
    }
  }

  private def vertexPositions(cell: Cell2DTriangle): (Int, Int, Int, Int, Int) = {
    val cx: Double = halfWidth + cell.pos.col.toDouble * halfWidth
    val cy = halfHeight + cell.pos.row.toDouble * height
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
