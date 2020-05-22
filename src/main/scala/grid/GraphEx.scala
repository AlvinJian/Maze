package grid

import java.awt
import java.awt.Graphics2D
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.canvas.{Canvas, GraphicsContext}
import com.sksamuel.scrimage.canvas.drawables.{FilledRect, Line}
import com.sksamuel.scrimage.color.{Color, RGBColor}
import com.sksamuel.scrimage.graphics.{Graphics2DUtils, RichGraphics2D}
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}

class GraphEx(val grid: GridEx) {
  private var _graph = Map[CellEx, Set[CellEx]]()

  def link(from: CellEx, to:CellEx): Option[GraphEx] =
    if (grid.isValid(from) && grid.isValid(to)) {
      var newGraph = _graph
      newGraph = GraphEx.linkHelper(newGraph, from, to)
      newGraph = GraphEx.linkHelper(newGraph, to, from)
      Some(GraphEx(this.grid, newGraph))
    } else None

  def isLinked(c1: CellEx, c2: CellEx): Boolean =
    if (_graph.contains(c1) && _graph(c1).contains(c2)) true
    else false

  def linkedCells(cell: CellEx): Option[Set[CellEx]] = _graph.get(cell)
  def linkedCells(r: Int, c: Int): Option[Set[CellEx]] = _graph.get(grid(r, c))
  def deadEnds: List[CellEx] =
    grid.filter((c)=>this.linkedCells(c).isDefined && this.linkedCells(c).get.size == 1).toList

  def dump(contentFunc: (CellEx) => String) = {
    val _out = new StringBuilder("+")
    _out.append(new String((0 until grid.col).flatMap(_ => "---+").toArray) + "\n")
    for (r <- 0 until grid.row) {
      val topSb = new StringBuilder("|")
      val bottomSb = new StringBuilder("+")
      for (c <- 0 until grid.col) {
        val cell: CellEx = grid(r, c)
        val connected = linkedCells(cell) match {
          case Some(cells) => cells
          case None => Set[CellEx]()
        }
        val bottomWall = cell.south match {
          case Some(south) => if (connected.contains(south)) "   " else "---"
          case _ => "---"
        }
        bottomSb.append(bottomWall).append("+")
        val eastWall = cell.east match {
          case Some(east) => if (connected.contains(east)) " " else "|"
          case _ => "|"
        }
        val sb = new StringBuilder(contentFunc(cell))
        while (sb.length() < 3) sb.append(' ')
        topSb.append(sb.toString()).append(eastWall)
      }
      _out.append(topSb.append('\n')).append(bottomSb.append('\n'))
    }
    _out.toString()
  }

  override def toString: String = dump(_ => "   ")

  @deprecated
  def toImage(cellSize: Int = 10,
              colorMapper: Option[CellEx=>RGBColor] = None,
              needPadding: Boolean = true): ImmutableImage = {
    val imgWidth = cellSize * grid.col
    val imgHeight = cellSize * grid.row
    var mutableImage = new MutableImage(new BufferedImage(imgWidth+1, imgHeight+1,
      BufferedImage.TYPE_INT_ARGB))
    mutableImage.fillInPlace(new awt.Color(255, 255, 255))

    val cellGraphics = new RichGraphics2D(mutableImage.awt().createGraphics())
    for (cell <- grid) {
      val x1 = cell.col * cellSize
      val y1 = cell.row * cellSize
      val x2 = (cell.col+1) * cellSize
      val y2 = (cell.row+1) * cellSize
      val cellColor = colorMapper match {
        case Some(func) => func(cell)
        case None => new RGBColor(255, 255, 255)
      }
      cellGraphics.setColor(cellColor)
      new FilledRect(x1, y1, x2, y2).draw(cellGraphics)
    }
    mutableImage.awt().flush()
    mutableImage = new MutableImage(mutableImage.awt())

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
        case Some(ecell) => !isLinked(cell, ecell)
        case _ => true
      }
      if (shouldDrawEast) new Line(x2, y1, x2, y2).draw(wallGraphics)
      val shouldDrawSouth = cell.south match {
        case Some(scell) => !isLinked(cell, scell)
        case _ => true
      }
      if (shouldDrawSouth) new Line(x1, y2, x2, y2).draw(wallGraphics)
    }

    if (needPadding) mutableImage.toImmutableImage.pad(5, awt.Color.GRAY)
    else mutableImage.toImmutableImage
  }
}

object GraphEx {
  private def apply(gridEx: GridEx, g: Map[CellEx, Set[CellEx]]): GraphEx = {
    val ret = new GraphEx(gridEx)
    ret._graph = g
    ret
  }

  private def linkHelper(iGraph: Map[CellEx, Set[CellEx]],
                         c1: CellEx, c2: CellEx): Map[CellEx, Set[CellEx]] = {
    if (iGraph.contains(c1)) iGraph.updated(c1, iGraph(c1) + c2)
    else iGraph + (c1 -> Set[CellEx](c2))
  }
}
