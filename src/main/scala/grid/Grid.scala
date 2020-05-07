package grid

class Grid(val row: Int, val col: Int) extends Iterable[Cell] {
  private val _cells: Vector[Vector[Cell]] = Vector.from(
    for (r <- 0 until row) yield Vector.from(
      for (c <- 0 until col) yield new Cell(r, c)
    )
  )
  configCells()

  private val _r = new scala.util.Random()

  def apply(row: Int, col: Int): Cell = _cells(row)(col)

  def randomCell(): Cell = {
    val n = _r.nextInt(row * col)
    _cells(n/row)(n%col)
  }

  override def iterator: Iterator[Cell] = new GridIterator(this)

  private def configCells() = {
    for (cell <- this) {
      if (cell.col+1 < col) {
        cell.east = _cells(cell.row)(cell.col+1)
      }
      if (cell.row+1 < row) {
        cell.south = _cells(cell.row+1)(cell.col)
      }
    }
  }

  private class GridIterator(val grid: Grid) extends Iterator[Cell] {
    private var _row = 0;
    private var _col = 0;

    override def hasNext: Boolean = _row < grid.row && _col < grid.col

    override def next(): Cell = {
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
