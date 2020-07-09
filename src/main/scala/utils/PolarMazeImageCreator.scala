package utils
import java.awt
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.canvas.drawables.Line
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DPolar, Graph, GraphEx, PolarGrid}

class PolarMazeImageCreator(override val graph: Graph,
                            override val cellSize: Int) extends MazeImageCreator {
  val polarGrid: PolarGrid = graph.grid.asInstanceOf[PolarGrid]

  override def baseImage: ImmutableImage = {
    val imgWidth = 2 * polarGrid.rows * cellSize
    ImmutableImage.filled(imgWidth+1, imgWidth+1,
      new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_RGB)
  }

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    val imageSize = 2 * polarGrid.rows * cellSize
    val center = imageSize / 2
    if (polarGrid.rows > 1) {
      for (polarCell <- polarGrid) {
        val r = polarCell.row
        val c = polarCell.col
        if (r > 0) {
          val theta: Double = (360.0 / polarGrid.columnCountAt(r).toDouble).toRadians
          val (ax, ay, bx, by, cx, cy, dx, dy) =
            calcPolarCellPosition(center, center, polarCell, cellSize, theta)

          val cellCw: Cell2D = polarCell.cw
          if (!graph.isLinked(polarCell, cellCw)) {
            new Line(ax, ay, bx, by).draw(wallGraphics)
          }
          val cellInward: Cell2D = polarCell.inward.get
          if (!graph.isLinked(polarCell, cellInward)) {
            new Line(ax, ay, cx, cy).draw(wallGraphics)
          }
          if (polarCell.outward.isEmpty) {
            new Line(bx, by, dx, dy).draw(wallGraphics)
          }
        }
      }
    } else {
      wallGraphics.drawArc(0, 0, imageSize, imageSize, 0, 360)
    }
    mutableImage.toImmutableImage
  }

  override def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    val center = baseImage.width / 2
    for (polarCell <- polarGrid) {
      val r = polarCell.row
      val c = polarCell.col
      cellGraphics.setColor(f(polarCell))
      if (r > 0) {
        val theta: Double = (360.0 / polarGrid.columnCountAt(r).toDouble).toRadians
        val (ax, ay, bx, by, cx, cy, dx, dy) =
          calcPolarCellPosition(center, center, polarCell, cellSize, theta)
        if (polarCell.outward.size > 1) {
          val outerTheta: Double = (360.0 / polarGrid.columnCountAt(r+1).toDouble).toRadians
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
          (cell) => {
            center + (radius * math.cos(theta * cell.col.toDouble)).toInt
          }
        ).toArray
        val ypoints: Array[Int] = polarCell.neighbors.map(
          (cell) => {
            center + (radius * math.sin(theta * cell.col.toDouble)).toInt
          }
        ).toArray
        cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
      }
    }
    mutableImage.toImmutableImage
  }

  private def calcPolarCellPosition(centerX: Int, centerY: Int,
                                    polarCell: Cell2DPolar,
                                    cellSize: Int, theta: Double):
  (Int, Int, Int, Int, Int, Int, Int, Int) = {
    val r = polarCell.row
    val c = polarCell.col
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
