package utils

import java.{awt, util}
import java.awt.{Color, Point}
import java.awt.image.BufferedImage

import algorithm.DistanceEx
import com.sksamuel.scrimage.canvas.GraphicsContext
import com.sksamuel.scrimage.canvas.drawables.{Arc, FilledArc, FilledPolygon, FilledRect, Line, Polygon}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DCart, Cell2DPolar, CellContainer, GraphEx, GridEx, MaskedGrid, PolarGrid}

object ImageUtils {
  private def calcRectCellPosition(cell2DCart: Cell2DCart, cellSize: Int): (Int, Int, Int, Int) = {
    val row = cell2DCart.row
    val col = cell2DCart.col
    val x1 = col * cellSize
    val y1 = row * cellSize
    val x2 = (col+1) * cellSize
    val y2 = (row+1) * cellSize
    (x1, y1, x2, y2)
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

  def createBaseImage(grid: CellContainer[Cell2D], cellSize: Int): ImmutableImage = grid match {
    case maskedGrid: MaskedGrid => {
      val imgWidth = grid.cols * cellSize
      val imgHeight = grid.rows * cellSize
      val baseImage = ImmutableImage.filled(imgWidth+1, imgHeight+1,
        new awt.Color(255, 255, 255), BufferedImage.TYPE_INT_RGB)
      val mutableImage = new MutableImage(baseImage.awt())
      val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
      // need to draw invalid cells
      for {
        r <- 0 until maskedGrid.rows
        c <- 0 until maskedGrid.cols
      } {
        val cell = maskedGrid(r,c)
        val (x1: Int, y1: Int, _, _) = calcRectCellPosition(cell, cellSize)
        if (!maskedGrid.isValid(cell)) cellGraphics.setColor(Color.BLACK)
        else cellGraphics.setColor(Color.WHITE)
        new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
      }
      mutableImage.toImmutableImage
    }
    case polarGrid: PolarGrid => {
      val imgWidth = 2 * polarGrid.rows * cellSize
      ImmutableImage.filled(imgWidth+1, imgWidth+1,
        new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_RGB)
    }
    case _ => {
      val imgWidth = grid.cols * cellSize
      val imgHeight = grid.rows * cellSize
      ImmutableImage.filled(imgWidth+1, imgHeight+1,
        new awt.Color(255, 255, 255), BufferedImage.TYPE_INT_RGB)
    }
  }

  def drawCartMaze(baseImage: ImmutableImage, graph: GraphEx, cellSize: Int): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)
    val grid = graph.grid.asInstanceOf[CellContainer[Cell2DCart]]

    for (cell <- grid) {
      val (x1, y1, x2, y2) = calcRectCellPosition(cell, cellSize)

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

  def drawPolarMaze(baseImage: ImmutableImage, graph: GraphEx, cellSize: Int): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)
    val polarGrid = graph.grid.asInstanceOf[PolarGrid]

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

  def drawColorCartGrid(baseImage: ImmutableImage, grid: CellContainer[Cell2DCart],
                        cellSize: Int, f: (Cell2D) => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- grid) {
      val x1 = cell.col * cellSize
      val y1 = cell.row * cellSize
      cellGraphics.setColor(f(cell))
      new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
    }
    mutableImage.toImmutableImage
  }

  def drawColorPolarGrid(baseImage: ImmutableImage, polarGrid: PolarGrid,
                         cellSize: Int, f: (Cell2D) => RGBColor): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
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

  def create(graph: GraphEx, cellSize: Int, padding: Option[Int]): ImmutableImage = {
    val start: (GraphEx)=>ImmutableImage = (graph)=> createBaseImage(graph.grid, cellSize)
    val pipeline = start.andThen((baseImage)=>baseImage.fill(awt.Color.WHITE))
      .andThen { (baseImage) =>
        graph.grid match {
          case gridEx: GridEx => drawCartMaze(baseImage, graph, cellSize)
          case maskedGrid: MaskedGrid => drawCartMaze(baseImage, graph, cellSize)
          case polarGrid: PolarGrid => drawPolarMaze(baseImage, graph, cellSize)
          case _ => ???
        }
      }
    val ret = pipeline(graph)
    if (padding.isDefined) ret.pad(padding.get, awt.Color.GRAY) else ret
  }

  def create(graph: GraphEx, cellSize: Int, f: (Cell2D)=>RGBColor,
             padding: Option[Int]): ImmutableImage = {
    val start: (GraphEx)=>ImmutableImage = (graph)=> createBaseImage(graph.grid, cellSize)
    val pipeline = start.andThen { (baseImage) =>
      graph.grid match {
        case gridEx: GridEx => drawColorCartGrid(baseImage, gridEx, cellSize, f)
        case maskedGrid: MaskedGrid => drawColorCartGrid(baseImage, maskedGrid, cellSize, f)
        case polarGrid: PolarGrid => drawColorPolarGrid(baseImage, polarGrid, cellSize, f)
        case _ => ???
      }
    }.andThen { (baseImage) =>
        graph.grid match {
          case gridEx: GridEx => drawCartMaze(baseImage, graph, cellSize)
          case maskedGrid: MaskedGrid => drawCartMaze(baseImage, graph, cellSize)
          case polarGrid: PolarGrid => drawPolarMaze(baseImage, graph, cellSize)
          case _ => ???
        }
      }
    val ret = pipeline(graph)
    if (padding.isDefined) ret.pad(padding.get, awt.Color.GRAY) else ret
  }

  def create(distanceEx: DistanceEx, cellSize: Int, padding: Option[Int]): ImmutableImage =
    create(distanceEx.graph, cellSize, distanceEx.colorMapper, padding)
}