package grid
import scala.util.Random

case class WeaveGrid(override val rows: Int,
                     override val cols: Int) extends CellContainer[Cell2DWeave] with Graph {
  override val grid: CellContainer[Cell2DWeave] = this
  private var _graph: Graph = new GraphEx(this)

  protected val data: Seq[Cell2DWeave] = Vector.from{
    for {
      r <- 0.until(rows)
      c <- 0.until(cols)
    } yield new Cell2DOverlay(this, r, c)
  }

  private var _hiddenCells = Map[Cell2DWeave, Cell2DWeave]()

  override def apply(r: Int, c: Int): Cell2DWeave = data(r * cols + c)

  def hiddenCell(r: Int, c: Int): Option[Cell2DHidden] = {
    _hiddenCells.get(apply(r, c)) match {
      case Some(value) => Some(value.asInstanceOf[Cell2DHidden])
      case None => None
    }
  }

  override def randomCell(r: Random): Cell2DWeave = {
    val id = r.nextInt(data.size + _hiddenCells.size)
    if (id < data.size) data(id)
    else {
      val arr = _hiddenCells.values.toArray
      arr(id % data.size)
    }
  }

  override def iterator: Iterator[Cell2DWeave] = {
    {
      data.toArray ++ _hiddenCells.values.toArray
    }.iterator
  }

  override def isValid(t: Cell2D): Boolean = {
    if (t.isInstanceOf[Cell2DHidden]) {
      val hidden = t.asInstanceOf[Cell2DHidden]
      _hiddenCells(hidden.overlay) == hidden
    } else super.isValid(t)
  }

  private def checkHiddenCellRequired(cell1: Cell2D,
                                      cell2: Cell2D): Option[Cell2DOverlay] = {
    if (!cell1.isInstanceOf[Cell2DOverlay] || !cell2.isInstanceOf[Cell2DOverlay]) return None
    val c1 = cell1.asInstanceOf[Cell2DOverlay]
    val c2 = cell2.asInstanceOf[Cell2DOverlay]
    val tmp: Option[Cell2DWeave] = {
      if (c1.north.isDefined && c1.north == c2.south) c1.north
      else if (c1.south.isDefined && c1.south == c2.north) c1.south
      else if (c1.west.isDefined && c1.west == c2.east) c1.west
      else if (c1.east.isDefined && c1.east == c2.west) c1.east
      else None
    }
    tmp match {
      case Some(value) if value.isInstanceOf[Cell2DOverlay] &&
        (value.isHorizontalLinked || value.isVerticalLinked) => Some(value.asInstanceOf[Cell2DOverlay])
      case _ => None
    }
  }

  override def link(from: Cell2D, to: Cell2D): Option[Graph] = {
    val optCell = checkHiddenCellRequired(from, to)
    if (optCell.isDefined && optCell.get.underneath.isEmpty) {
      val h = new Cell2DHidden(optCell.get)
      _hiddenCells = _hiddenCells + (optCell.get -> h)
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
}
