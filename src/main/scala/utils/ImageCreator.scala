package utils

import java.awt
import java.awt.Color
import java.awt.image.BufferedImage

import algorithm.DistanceEx
import com.sksamuel.scrimage.canvas.drawables.{FilledRect, Line}
import com.sksamuel.scrimage.color.RGBColor
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}
import com.sksamuel.scrimage.graphics.RichGraphics2D
import grid.{CellEx, GraphEx, GridContainer, GridEx, MaskedGrid}

sealed abstract class ImageCreator(val baseGrid: GridContainer[CellEx],
                                   val cellSize: Int) {
  val baseImage: ImmutableImage = {
    val imgWidth = cellSize * baseGrid.col
    val imgHeight = cellSize * baseGrid.row
    val image = ImmutableImage.filled(imgWidth+1, imgHeight+1,
      new awt.Color(255, 255, 255, 0), BufferedImage.TYPE_INT_ARGB)
    image
  }
  def drawOn(baseImage: ImmutableImage): ImmutableImage
  def create(): ImmutableImage = drawOn(baseImage)
}

class Background(grid: GridContainer[CellEx], cellSize: Int) extends ImageCreator(grid, cellSize) {
  override def drawOn(baseImage: ImmutableImage): ImmutableImage = {
    baseGrid match {
      case normal: GridEx => baseImage.fill(awt.Color.WHITE)
      case masked: MaskedGrid => {
        val mutableImage = new MutableImage(baseImage.awt())
        val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
        // need to draw invalid cells
        for {
          r <- 0 until masked.row
          c <- 0 until masked.col
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
      case _ => baseImage
    }
  }
}

class ColoredImageCreator(grid: GridContainer[CellEx], cellSize: Int,
                          mapper: (CellEx)=>RGBColor) extends ImageCreator(grid, cellSize) {
  override def drawOn(baseImage: ImmutableImage): ImmutableImage = {
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
    val mutableImage = new MutableImage(baseImage.awt())

    val wallColor = new RGBColor(0, 0, 0)
    val wallGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    wallGraphics.setColor(wallColor)

    for (cell <- baseGrid) {
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