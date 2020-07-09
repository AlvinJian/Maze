package grid
import scala.util.Random

case class WeaveGrid(override val rows: Int,
                     override val cols: Int) extends CellContainer[Cell2DOverlay] with Graph {
  override val grid: CellContainer[Cell2DOverlay] = this
  private var _graph: Graph = new GraphEx(this)
  private var _hiddenCells = Map[Cell2DOverlay, Cell2DHidden]()

  override def apply(r: Int, c: Int): Cell2DOverlay = ???

  override def randomCell(r: Random): Cell2DOverlay = ???

  override def iterator: Iterator[Cell2DOverlay] = ???

  // TODO
  override def link(from: Cell2D, to: Cell2D): Option[Graph] = {
    _graph = _graph.link(from, to) match {
      case Some(value) => value
      case None => _graph
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

    def canTunnelNorth: Boolean = ???

    def canTunnelSouth: Boolean = ???

    def canTunnelEast: Boolean = ???

    def canTunnelWest: Boolean = ???

    override def north: Option[Cell2DOverlay] = ???

    override def south: Option[Cell2DOverlay] = ???

    override def east: Option[Cell2DOverlay] = ???

    override def west: Option[Cell2DOverlay] = ???

    override def hiddenCell: Option[Cell2DHidden] = ???
  }

  private class Cell2DHiddenImpl(override val overlay: Cell2DOverlay) extends Cell2DHidden {
    override type T = Cell2DHidden

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
