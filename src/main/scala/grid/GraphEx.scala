package grid

class GraphEx(val grid: GridContainer[Cell2D]) {
  private var _graph = Map[Cell2D, Set[Cell2D]]()

  def link(from: Cell2D, to:Cell2D): Option[GraphEx] =
    if (grid.isValid(from) && grid.isValid(to)) {
      var newGraph = _graph
      newGraph = GraphEx.linkHelper(newGraph, from, to)
      newGraph = GraphEx.linkHelper(newGraph, to, from)
      Some(GraphEx(this.grid, newGraph))
    } else None

  def isLinked(c1: Cell2D, c2: Cell2D): Boolean =
    if (_graph.contains(c1) && _graph(c1).contains(c2)) true
    else false

  def linkedCells(cell: Cell2D): Option[Set[Cell2D]] = _graph.get(cell)
  def linkedCells(r: Int, c: Int): Option[Set[Cell2D]] = _graph.get(grid(r, c))
  def deadEnds: List[Cell2D] =
    grid.filter((c)=>this.linkedCells(c).isDefined && this.linkedCells(c).get.size == 1).toList

  @deprecated
  def dump(contentFunc: (Cell2DCart) => String) = {
    val _out = new StringBuilder("+")
    _out.append(new String((0 until grid.cols).flatMap(_ => "---+").toArray) + "\n")
    for (r <- 0 until grid.rows) {
      val topSb = new StringBuilder("|")
      val bottomSb = new StringBuilder("+")
      for (c <- 0 until grid.cols) {
        val cell: Cell2DCart = grid(r, c).asInstanceOf[Cell2DCart]
        val connected = linkedCells(cell) match {
          case Some(cells) => cells
          case None => Set[Cell2DCart]()
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
  private def apply(grid: GridContainer[Cell2D], g: Map[Cell2D, Set[Cell2D]]): GraphEx = {
    val ret = new GraphEx(grid)
    ret._graph = g
    ret
  }

  private def linkHelper(iGraph: Map[Cell2D, Set[Cell2D]],
                         c1: Cell2D, c2: Cell2D): Map[Cell2D, Set[Cell2D]] = {
    if (iGraph.contains(c1)) iGraph.updated(c1, iGraph(c1) + c2)
    else iGraph + (c1 -> Set[Cell2D](c2))
  }
}
