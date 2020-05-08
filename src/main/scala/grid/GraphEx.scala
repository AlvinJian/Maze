package grid

import com.sksamuel.scrimage.ImmutableImage

class GraphEx(val grid: GridEx) {
  private var _graph = Map[CellEx, Set[CellEx]]()

  def link(from: CellEx, to:CellEx): Option[GraphEx] =
    if (grid.contains(from) && grid.contains(to)) {
      var newGraph = _graph
      newGraph = GraphEx.linkHelper(newGraph, from, to)
      newGraph = GraphEx.linkHelper(newGraph, to, from)
      Some(GraphEx(this.grid, newGraph))
    }
    else None

  def isLinked(c1: CellEx, c2: CellEx): Boolean =
    if (_graph.contains(c1) && _graph(c1).contains(c2)) true
    else false

  def linkedCells(cell: CellEx): Option[Set[CellEx]] = _graph.get(cell)
  def linkedCells(r: Int, c: Int): Option[Set[CellEx]] = _graph.get(grid(r, c))

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
        topSb.append(contentFunc(cell)).append(eastWall)
      }
      _out.append(topSb.append('\n')).append(bottomSb.append('\n'))
    }
    _out.toString()
  }

  override def toString: String = dump(_ => "   ")

  def asImage(cellSize: Int = 10): ImmutableImage = ???
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
