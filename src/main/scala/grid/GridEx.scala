package grid

import scala.util.Random

trait CellEx {
  def row: Int
  def col: Int
  def north: Option[CellEx]
  def south: Option[CellEx]
  def east: Option[CellEx]
  def west: Option[CellEx]
  def neighbors: List[CellEx]
}

private[grid] class CellExImpl(val row: Int, val col: Int, grid: GridEx) extends CellEx {
  override def north: Option[CellEx] = if (row-1 >= 0) Some(grid(row-1, col)) else None

  override def south: Option[CellEx] = if (row+1 < grid.row) Some(grid(row+1, col)) else None

  override def east: Option[CellEx] = if (col+1 < grid.col) Some(grid(row, col+1)) else None

  override def west: Option[CellEx] = if (col-1 >= 0) Some(grid(row, col-1)) else None

  override def neighbors: List[CellEx] = List(this.north, this.south, this.east, this.west).flatten
}

class GridEx(val row: Int, val col: Int) extends Iterable[CellEx] {
  private val data = Vector.from(
    for {
      r <- 0 until row
      c <- 0 until col
    } yield new CellExImpl(r, c, this)
  )

  def apply(r: Int, c: Int): CellEx = data(r * col + c)
  def contains(cell: CellEx): Boolean = {
    if (cell.row < this.row && cell.col < this.col) this(cell.row, cell.col) == cell
    else false
  }

  def randomCell(r: Random): CellEx = data(r.nextInt(data.size))

  override def iterator: Iterator[CellEx] = new GridIterator(this)

  private class GridIterator(val grid: GridEx) extends Iterator[CellEx] {
    private var _row = 0;
    private var _col = 0;

    override def hasNext: Boolean = _row < grid.row && _col < grid.col

    override def next(): CellEx = {
      val c = grid(_row, _col)
      _inc()
      c
    }

    private def _inc() = {
      if (_col+1 < grid.col) {
        _col += 1
      } else {
        _col = 0
        _row += 1
      }
    }
  }
}
