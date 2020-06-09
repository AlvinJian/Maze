package utils

import java.awt
import java.awt.Color
import java.awt.image.BufferedImage

import algorithm.DistanceEx
import com.sksamuel.scrimage.canvas.drawables.{Arc, FilledRect, Line}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{Cell2D, Cell2DCart, Cell2DPolar, CellContainer, GraphEx, GridEx, MaskedGrid, PolarGrid}

@deprecated
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

@deprecated
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

@deprecated
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

@deprecated
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
    if (polarGrid.rows > 1) {
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
}

object ImageCreator {
  @deprecated
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

  def createBaseImage(grid: CellContainer[Cell2D], cellSize: Int): ImmutableImage = grid match {
    case maskedGrid: MaskedGrid => {
      val imgWidth = grid.cols * cellSize
      val imgHeight = grid.rows * cellSize
      val baseImage = ImmutableImage.filled(imgWidth+1, imgHeight+1,
        new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)
      val mutableImage = new MutableImage(baseImage.awt())
      val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
      cellGraphics.setColor(Color.BLACK)
      // need to draw invalid cells
      for {
        r <- 0 until maskedGrid.rows
        c <- 0 until maskedGrid.cols
      } {
        val cell = maskedGrid(r,c)
        if (!maskedGrid.isValid(cell)) {
          val x1 = cell.col * cellSize
          val y1 = cell.row * cellSize
          new FilledRect(x1, y1, cellSize, cellSize).draw(cellGraphics)
        }
      }
      mutableImage.toImmutableImage
    }
    case polarGrid: PolarGrid => {
      val imgWidth = 2 * polarGrid.rows * cellSize
      ImmutableImage.filled(imgWidth+1, imgWidth+1,
        new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)
    }
    case _ => {
      val imgWidth = grid.cols * cellSize
      val imgHeight = grid.rows * cellSize
      ImmutableImage.filled(imgWidth+1, imgHeight+1,
        new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)
    }
  }

  def drawCartMaze(baseImage: ImmutableImage, graph: GraphEx, cellSize: Int): ImmutableImage = {
    val mutableImage = new MutableImage(baseImage.awt())
    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)
    val grid = graph.grid.asInstanceOf[CellContainer[Cell2DCart]]

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

  // TODO
  def drawColorPolarGrid(baseImage: ImmutableImage, grid: PolarGrid,
                         cellSize: Int, f: (Cell2D) => RGBColor): ImmutableImage = ???

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