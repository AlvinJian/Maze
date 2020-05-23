package grid

import java.awt
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.canvas.drawables.{FilledRect, Line}
import com.sksamuel.scrimage.color.{Color, RGBColor}
import com.sksamuel.scrimage.graphics.{Graphics2DUtils, RichGraphics2D}
import com.sksamuel.scrimage.{ImmutableImage, MutableImage}

class GraphEx(val grid: GridContainer[CellEx]) {
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
    _out.append(new String((0 until grid.cols).flatMap(_ => "---+").toArray) + "\n")
    for (r <- 0 until grid.rows) {
      val topSb = new StringBuilder("|")
      val bottomSb = new StringBuilder("+")
      for (c <- 0 until grid.cols) {
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
}

object GraphEx {
  private def apply(grid: GridContainer[CellEx], g: Map[CellEx, Set[CellEx]]): GraphEx = {
    val ret = new GraphEx(grid)
    ret._graph = g
    ret
  }

  private def linkHelper(iGraph: Map[CellEx, Set[CellEx]],
                         c1: CellEx, c2: CellEx): Map[CellEx, Set[CellEx]] = {
    if (iGraph.contains(c1)) iGraph.updated(c1, iGraph(c1) + c2)
    else iGraph + (c1 -> Set[CellEx](c2))
  }
}
