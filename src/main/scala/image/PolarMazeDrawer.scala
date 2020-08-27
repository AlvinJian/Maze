package image

import java.awt
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DPolar, Maze, PolarGrid, Position2D}

private[image] class PolarMazeDrawer(val grid: PolarGrid,
                                     m: Maze[Cell2DPolar],
                                     cSize: Int) extends Drawer {
  override type M = Maze[Cell2DPolar]

  override def maze: M = m

  override def cellSize: Int = cSize

  override def baseImage: ImmutableImage = {
    val imgWidth = 2 * grid.radius * cellSize
    ImmutableImage.filled(imgWidth+1, imgWidth+1,
      new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_RGB)
  }

  override protected def drawWalls(g2: RichGraphics2D): Unit = {
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = g2
    wallGraphics.setColor(wallColor)

    val imageSize = 2 * grid.radius * cellSize
    val center = imageSize / 2
    if (grid.radius > 1) {
      for (polarCell <- maze) {
        val r = polarCell.pos.row
        val c = polarCell.pos.col
        if (r > 0) {
          val theta: Double = (360.0 / grid.countAt(r).toDouble).toRadians
          val (ax, ay, bx, by, cx, cy, dx, dy) =
            calcPolarCellPosition(center, center, polarCell, cellSize, theta)

          val cellCw = polarCell.cw
          if (!maze.linkedBy(polarCell.pos).contains(cellCw)) {
            wallGraphics.drawLine(ax, ay, bx, by)
          }
          val cellInward = polarCell.inward.get // if we're not in the central cell, we're safe here
          if (!maze.linkedBy(polarCell.pos).contains(cellInward)) {
            wallGraphics.drawLine(ax, ay, cx, cy)
          }
          if (polarCell.outward.isEmpty) {
            wallGraphics.drawLine(bx, by, dx, dy)
          }
        }
      }
    } else {
      wallGraphics.drawArc(0, 0, imageSize, imageSize, 0, 360)
    }
  }

  override protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val cellGraphics = g2
    val center = baseImage.width / 2
    for (polarCell <- maze) {
      val r = polarCell.pos.row
      val c = polarCell.pos.col
      cellGraphics.setColor(f(polarCell.pos))
      if (r > 0) {
        val theta: Double = (360.0 / grid.countAt(r).toDouble).toRadians
        val (ax, ay, bx, by, cx, cy, dx, dy) =
          calcPolarCellPosition(center, center, polarCell, cellSize, theta)
        if (polarCell.outward.size > 1) {
          val outerTheta: Double = (360.0 / grid.countAt(r+1).toDouble).toRadians
          val (_, _, _, _, ex, ey, _, _) =
            calcPolarCellPosition(center, center, polarCell.outward.head, cellSize, outerTheta)
          val xpoints: Array[Int] = Array(ax, bx, ex, dx, cx)
          val ypoints: Array[Int] = Array(ay, by, ey, dy, cy)
          cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
        } else {
          val xpoints: Array[Int] = Array(ax, bx, dx, cx)
          val ypoints: Array[Int] = Array(ay, by, dy, cy)
          cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
        }
      } else {
        val radius: Int = cellSize
        val theta: Double = (360.0 / polarCell.outward.size.toDouble).toRadians
        val xpoints: Array[Int] = polarCell.neighbors.map(
          cell => {
            center + (radius * math.cos(theta * cell.pos.col.toDouble)).toInt
          }
        ).toArray
        val ypoints: Array[Int] = polarCell.neighbors.map(
          cell => {
            center + (radius * math.sin(theta * cell.pos.col.toDouble)).toInt
          }
        ).toArray
        cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
      }
    }
  }

  def calcPolarCellPosition(centerX: Int, centerY: Int,
                            polarCell: Cell2DPolar,
                            cellSize: Int, theta: Double):
  (Int, Int, Int, Int, Int, Int, Int, Int) = {
    val r = polarCell.pos.row
    val c = polarCell.pos.col
    val innRadius = r * cellSize
    val outRadius = (r+1) * cellSize
    val thetaCcw = (c+1).toDouble * theta
    val thetaCw = c.toDouble * theta

    val ax = centerX + (innRadius.toDouble * math.cos(thetaCw)).toInt
    val ay = centerY + (innRadius.toDouble * math.sin(thetaCw)).toInt
    val bx = centerX + (outRadius.toDouble * math.cos(thetaCw)).toInt
    val by = centerY + (outRadius.toDouble * math.sin(thetaCw)).toInt
    val cx = centerX + (innRadius.toDouble * math.cos(thetaCcw)).toInt
    val cy = centerY + (innRadius.toDouble * math.sin(thetaCcw)).toInt
    val dx = centerX + (outRadius.toDouble * math.cos(thetaCcw)).toInt
    val dy = centerY + (outRadius.toDouble * math.sin(thetaCcw)).toInt

    (ax, ay, bx, by, cx, cy, dx, dy)
  }
}
