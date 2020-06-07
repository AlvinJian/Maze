package utils

import java.awt
import java.awt.Color
import java.awt.image.BufferedImage

import algorithm.DistanceEx
import com.sksamuel.scrimage.canvas.drawables.{Arc, FilledRect, Line}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DCart, CellContainer, GraphEx, GridEx, MaskedGrid, PolarGrid}

sealed abstract class ImageCreator(val baseGrid: CellContainer[Cell2D],
                                   val cellSize: Int) {
  val baseImage: ImmutableImage = baseGrid match {
    case GridEx(rows, cols) => createRectImage(rows * cellSize, cols * cellSize)
    case MaskedGrid(rows, cols, blacklist) => createRectImage(rows * cellSize, cols * cellSize)
    case PolarGrid(rows) =>createRectImage(rows * cellSize)
    case _ => ???
  }
  def drawOn(baseImage: ImmutableImage): ImmutableImage
  def create(): ImmutableImage = drawOn(baseImage)

  protected def createRectImage(imgWidth: Int, imgHeight: Int): ImmutableImage =
    ImmutableImage.filled(imgWidth+1, imgHeight+1, new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)

  protected def createRectImage(radius: Int): ImmutableImage = {
    val imgWidth = 2 * radius + 1
    ImmutableImage.filled(imgWidth, imgWidth, new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)
  }
}

class Background(grid: CellContainer[Cell2D], cellSize: Int)
  extends ImageCreator(grid, cellSize) {
  override def drawOn(baseImage: ImmutableImage): ImmutableImage = {
    baseGrid match {
      case masked: MaskedGrid => {
        val mutableImage = new MutableImage(baseImage.awt())
        val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
        // need to draw invalid cells
        for {
          r <- 0 until masked.rows
          c <- 0 until masked.cols
        } {
          val cell = masked(r,c)
          if (masked.isValid(cell)) {
            cellGraphics.setColor(Color.WHITE)
          } else {
            cellGraphics.setColor(Color.BLACK)
          }
          val x1 = cell.col * cellSize
          val y1 = cell.row * cellSize
          new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
        }
        mutableImage.toImmutableImage
      }
      case _ => baseImage.fill(awt.Color.WHITE)
    }
  }
}

class ColoredImageCreator(grid: CellContainer[Cell2D], cellSize: Int,
                          mapper: (Cell2D) => RGBColor)
  extends ImageCreator(grid, cellSize) {
  override def drawOn(baseImage: ImmutableImage): ImmutableImage = baseGrid match {
    case grid : GridEx => drawCartGrid
    case maskedGrid: MaskedGrid => drawCartGrid
    case polarGrid: PolarGrid => ???
    case _ => ???
  }

  protected def drawCartGrid = {
    val mutableImage = new MutableImage(baseImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- baseGrid) {
      val x1 = cell.col * cellSize
      val y1 = cell.row * cellSize
      cellGraphics.setColor(mapper(cell))
      new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
    }
    mutableImage.toImmutableImage
  }
}

class MazeImageCreator(val graph: GraphEx, cellSize: Int) extends ImageCreator(graph.grid, cellSize) {
  override def drawOn(baseImage: ImmutableImage): ImmutableImage = {
    graph.grid match {
      case grid: GridEx => drawCartGrid(baseImage, grid)
      case maskedGrid: MaskedGrid => drawCartGrid(baseImage, maskedGrid)
      case polarGrid: PolarGrid => drawPolarGrid(baseImage, polarGrid)
      case _ => ???
    }
  }

  private def drawCartGrid(baseImage: ImmutableImage, grid: CellContainer[Cell2DCart]): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    for (cell <- grid) {
      val x1 = cell.col * cellSize
      val y1 = cell.row * cellSize
      val x2 = (cell.col+1) * cellSize
      val y2 = (cell.row+1) * cellSize

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

  private def drawPolarGrid(baseImage: ImmutableImage, polarGrid: PolarGrid): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    val polarGrid = baseGrid.asInstanceOf[PolarGrid]
    val imageSize = 2 * polarGrid.rows * cellSize
    val center = imageSize / 2
    for (polarCell <- polarGrid) {
      val r = polarCell.row
      val c = polarCell.col
      if (r > 0) {
        val theta: Double = (360.0 / polarGrid.columnCountAt(r).toDouble).toRadians
        val innRadius = r * cellSize
        val outRadius = (r+1) * cellSize
        val thetaCcw = (c+1).toDouble * theta
        val thetaCw = c.toDouble * theta

        val ax = center + (innRadius.toDouble * math.cos(thetaCw)).toInt
        val ay = center + (innRadius.toDouble * math.sin(thetaCw)).toInt
        val bx = center + (outRadius.toDouble * math.cos(thetaCw)).toInt
        val by = center + (outRadius.toDouble * math.sin(thetaCw)).toInt
        val cx = center + (innRadius.toDouble * math.cos(thetaCcw)).toInt
        val cy = center + (innRadius.toDouble * math.sin(thetaCcw)).toInt
        val dx = center + (outRadius.toDouble * math.cos(thetaCcw)).toInt
        val dy = center + (outRadius.toDouble * math.sin(thetaCcw)).toInt

        val cellCw: Cell2D = polarCell.cw.get // cw is always valid
        if (!graph.isLinked(polarCell, cellCw)) {
          new Line(ax, ay, bx, by).draw(wallGraphics)
        }
        val cellInward: Cell2D = polarCell.inward.get
        if (!graph.isLinked(polarCell, cellInward)) {
//          wallGraphics.drawArc(center-innRadius, center-innRadius, innRadius*2, innRadius*2,
//            thetaCw.toDegrees.toInt, theta.toDegrees.toInt)
          new Line(ax, ay, cx, cy).draw(wallGraphics)
        }
        if (polarCell.outward.isEmpty) {
          new Line(bx, by, dx, dy).draw(wallGraphics)
        }
      }
    }
//    wallGraphics.drawArc(0, 0, imageSize, imageSize, 0, 360)
    mutableImage.toImmutableImage
  }
}

object ImageCreator {
  def batch(padding: Option[Int]=None)(imageCreators: ImageCreator*): ImmutableImage = {
    var ret = imageCreators.foldLeft[Option[ImmutableImage]](None)(
      (prev: Option[ImmutableImage], creator: ImageCreator) => {
        var img = creator.create()
        if (prev.isDefined) img = prev.get.overlay(img)
        Some(img)
      })
    if (padding.isDefined) ret = Some(ret.get.pad(padding.get, awt.Color.GRAY))
    ret.get
  }
  def create(graph: GraphEx, cellSize: Int, padding: Option[Int]): ImmutableImage = {
    ImageCreator.batch(padding)(
      new Background(graph.grid, cellSize),
      new MazeImageCreator(graph, cellSize)
    )
  }
  def create(distMap: DistanceEx, cellSize: Int, padding: Option[Int]): ImmutableImage = {
    ImageCreator.batch(padding)(
      new ColoredImageCreator(distMap.graph.grid, cellSize, distMap.colorMapper),
      new MazeImageCreator(distMap.graph, cellSize)
    )
  }
}