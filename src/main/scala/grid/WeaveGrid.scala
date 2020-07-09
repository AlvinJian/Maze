package grid
import scala.util.Random

case class WeaveGrid(override val rows: Int,
                     override val cols: Int) extends CellContainer[Cell2DOverlay] with Graph {
  override val grid: CellContainer[Cell2DOverlay] = this
  private var _graph: Graph = new GraphEx(this)

  protected val data: Seq[Cell2DOverlay] = Vector.from{
    for {
      r <- 0.until(rows)
      c <- 0.until(cols)
    } yield new Cell2DOverlayImpl(r, c)
  }

  private var _hiddenCells = Map[Cell2DOverlay, Cell2DHidden]()

  override def apply(r: Int, c: Int): Cell2DOverlay = data(r * cols + c)

  override def randomCell(r: Random): Cell2DOverlay = data(r.nextInt(data.size))

  override def iterator: Iterator[Cell2DOverlay] = data.iterator

  private def checkHiddenCellRequired(cell1: Cell2D,
                                      cell2: Cell2D): Option[Cell2DOverlay] = {
    if (!cell1.isInstanceOf[Cell2DOverlay] || !cell2.isInstanceOf[Cell2DOverlay]) return None
    val c1 = cell1.asInstanceOf[Cell2DOverlay]
    val c2 = cell2.asInstanceOf[Cell2DOverlay]
    if (c1.north == c2.south) c1.north
    else if (c1.south == c2.north) c1.south
    else if (c1.west == c2.east) c1.west
    else if (c1.east == c2.west) c1.east
    else None
  }

  override def link(from: Cell2D, to: Cell2D): Option[Graph] = {
    val optCell = checkHiddenCellRequired(from, to)
    if (optCell.isDefined) {
      val _ = new Cell2DHiddenImpl(optCell.get)
    } else {
      _graph = _graph.link(from, to) match {
        case Some(value) => value
        case None => _graph
      }
    }
    Some(this)
  }

  override def isLinked(c1: Cell2D, c2: Cell2D): Boolean = _graph.isLinked(c1, c2)

  override def linkedCells(cell: Cell2D): Set[Cell2D] = _graph.linkedCells(cell)

  override def deadEnds: List[Cell2D] = _graph.deadEnds

  def reset(): Graph = {
    val old = _graph
    _graph = new GraphEx(this)
    old
  }

  private class Cell2DOverlayImpl(override val row: Int,
                                  override val col: Int) extends Cell2DOverlay {
    override type T = Cell2DOverlay

    val outer: WeaveGrid = WeaveGrid.this

    def canTunnelNorth: Boolean = {
      north.isDefined && north.get.north.isDefined && {
        val n = north.get.asInstanceOf[this.type]
        n.hasHorizontalLink
      }
    }

    def canTunnelSouth: Boolean = {
      south.isDefined && south.get.south.isDefined && {
        val s = south.get.asInstanceOf[this.type]
        s.hasHorizontalLink
      }
    }

    def canTunnelEast: Boolean = {
      east.isDefined && east.get.east.isDefined && {
        val e = east.get.asInstanceOf[this.type]
        e.hasVerticalLink
      }
    }

    def canTunnelWest: Boolean = {
      west.isDefined && west.get.west.isDefined && {
        val w = west.get.asInstanceOf[this.type]
        w.hasVerticalLink
      }
    }

    def hasHorizontalLink: Boolean = {
      if (east.isDefined && west.isDefined) {
        outer._graph.isLinked(this, east.get) &&
          outer._graph.isLinked(this, west.get) && {
          north.isEmpty || !outer._graph.isLinked(this, north.get)
        } && {
          south.isEmpty || !outer._graph.isLinked(this, south.get)
        }
      } else false
    }

    def hasVerticalLink: Boolean = {
      if (north.isDefined && south.isDefined) {
        outer._graph.isLinked(this, north.get) &&
          outer._graph.isLinked(this, south.get) && {
          east.isEmpty || outer._graph.isLinked(this, east.get)
        } && {
          west.isEmpty || outer._graph.isLinked(this, west.get)
        }
      } else false
    }

    override def north: Option[Cell2DOverlay] = ???

    override def south: Option[Cell2DOverlay] = ???

    override def east: Option[Cell2DOverlay] = ???

    override def west: Option[Cell2DOverlay] = ???

    override def underneath: Option[Cell2DHidden] = outer._hiddenCells.get(this)

    override def neighbors: List[Cell2DOverlay] = {
      var neighbors = super.neighbors
      neighbors = neighbors ++ {
        if (canTunnelNorth) List(north.get.north).flatten else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelSouth) List(south.get.south).flatten else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelEast) List(east.get.east).flatten else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelWest) List(west.get.west).flatten else Nil
      }
      neighbors
    }
  }

  private class Cell2DHiddenImpl(override val overlay: Cell2DOverlay) extends Cell2DHidden {
    override type T = Cell2DHidden

    val outer: WeaveGrid = WeaveGrid.this
    outer._hiddenCells = outer._hiddenCells + (overlay -> this)

    override val row: Int = overlay.row
    override val col: Int = overlay.col

    override def north: Option[Cell2DHidden] = ???

    override def south: Option[Cell2DHidden] = ???

    override def east: Option[Cell2DHidden] = ???

    override def west: Option[Cell2DHidden] = ???

    override def isHorizontalLink: Boolean = ???

    override def isVerticalLink: Boolean = ???
  }
}
