package image

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import maze.{Cell2DHex, Maze, Position2D, RectGrid}

private[image] class HexMazeDrawer(val grid: RectGrid,
                                   val m: Maze[Cell2DHex],
                                   size: Int) extends Drawer {
  override type M = Maze[Cell2DHex]

  private val aSize = cellSize / 2
  private val bSize = (cellSize.toDouble * math.sqrt(3.0) / 2.0).toInt
  private val width = cellSize * 2
  private val height = bSize * 2
  val imgWidth: Int = 3 * aSize * grid.cols + aSize
  val imgHeight: Int = height * grid.rows + bSize

  override def maze: M = m

  override def cellSize: Int = size

  override def baseImage: ImmutableImage =
    ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE)

  override protected def drawWalls(g2: RichGraphics2D): Unit = {
    val wallGraphics = g2
    wallGraphics.setColor(java.awt.Color.BLACK)
    for (cell <- maze) {
      val (xFw, xNw, xNe, xFe, yN, yM, yS) = hexCellPositions(cell)
      if (cell.southwest.isEmpty) {
        wallGraphics.drawLine(xFw, yM, xNw, yS)
      }
      if (cell.northwest.isEmpty) {
        wallGraphics.drawLine(xFw, yM, xNw, yN)
      }
      if (cell.north.isEmpty) {
        wallGraphics.drawLine(xNw, yN, xNe, yN)
      }
      if (cell.northeast.isEmpty || !cell.linked.contains(cell.northeast.get)) {
        wallGraphics.drawLine(xNe, yN, xFe, yM)
      }
      if (cell.southeast.isEmpty || !cell.linked.contains(cell.southeast.get)) {
        wallGraphics.drawLine(xFe, yM, xNe, yS)
      }
      if (cell.south.isEmpty || !cell.linked.contains(cell.south.get)) {
        wallGraphics.drawLine(xNe, yS, xNw, yS)
      }
    }
  }

  override protected def drawCells(g2: RichGraphics2D, f: Position2D => RGBColor): Unit = {
    val cellGraphics = g2
    for (cell <- maze) {
      cellGraphics.setColor(f(cell.pos))
      val (xFw, xNw, xNe, xFe, yN, yM, yS) = hexCellPositions(cell)
      val xpoints = Array(xFw, xNw, xNe, xFe, xNe, xNw)
      val ypoints = Array(yM, yN, yN, yM, yS, yS)
      cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
    }
  }

  protected def hexCellPositions(cell: Cell2DHex): (Int,Int,Int,Int,Int,Int,Int) = {
    val cx = cellSize + 3 * cell.pos.col * aSize
    val cy = bSize + cell.pos.row * height + {
      if (cell.pos.col % 2 == 1) bSize else 0
    }
    val xFw = cx - cellSize
    val xNw = cx - aSize
    val xNe = cx + aSize
    val xFe = cx + cellSize
    val yN = cy - bSize
    val yM = cy
    val yS = cy + bSize
    (xFw, xNw, xNe, xFe, yN, yM, yS)
  }
}
