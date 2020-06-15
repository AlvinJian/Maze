package utils
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DHex, CellContainer, GraphEx}

class HexMazeImageCreator(override val graph: GraphEx,
                          override val cellSize: Int) extends MazeImageCreator {
  val hexGrid = graph.grid.asInstanceOf[CellContainer[Cell2DHex]]
  private val aSize = cellSize / 2
  private val bSize = (cellSize.toDouble * math.sqrt(3.0) / 2.0).toInt
  private val width = cellSize * 2
  private val height = bSize * 2
  val imgWidth: Int = 3 * aSize * hexGrid.cols + aSize
  val imgHeight: Int = height * hexGrid.rows + bSize

  override def baseImage: ImmutableImage = ImmutableImage.filled(imgWidth+1, imgHeight+1, java.awt.Color.WHITE)

  override def drawMazeWalls(prevImage: ImmutableImage): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(java.awt.Color.BLACK)
    for (cell <- hexGrid) {
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
      if (cell.northeast.isEmpty ||
        !graph.isLinked(cell, cell.northeast.get)) {
        wallGraphics.drawLine(xNe, yN, xFe, yM)
      }
      if (cell.southeast.isEmpty ||
        !graph.isLinked(cell, cell.southeast.get)) {
        wallGraphics.drawLine(xFe, yM, xNe, yS)
      }
      if (cell.south.isEmpty ||
        !graph.isLinked(cell, cell.south.get)) {
        wallGraphics.drawLine(xNe, yS, xNw, yS)
      }
    }
    mutableImage.toImmutableImage
  }

  override def drawColoredCells(prevImage: ImmutableImage, f: Cell2D => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(prevImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- hexGrid) {
      cellGraphics.setColor(f(cell))
      val (xFw, xNw, xNe, xFe, yN, yM, yS) = hexCellPositions(cell)
      val xpoints = Array(xFw, xNw, xNe, xFe, xNe, xNw)
      val ypoints = Array(yM, yN, yN, yM, yS, yS)
      cellGraphics.fillPolygon(xpoints, ypoints, xpoints.length)
    }
    mutableImage.toImmutableImage
  }

  private def hexCellPositions(cell: Cell2DHex): (Int,Int,Int,Int,Int,Int,Int) = {
    val cx = cellSize + 3 * cell.col * aSize
    val cy = bSize + cell.row * height + {
      if (cell.col % 2 == 1) bSize else 0
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
