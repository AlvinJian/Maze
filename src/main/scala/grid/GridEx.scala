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
  override def north: Option[CellEx] = grid.adjacencyOf(this, NorthDir)

  override def south: Option[CellEx] = grid.adjacencyOf(this, SouthDir)

  override def east: Option[CellEx] = grid.adjacencyOf(this, EastDir)

  override def west: Option[CellEx] = grid.adjacencyOf(this, WestDir)

  override def neighbors: List[CellEx] = List(this.north, this.south, this.east, this.west).flatten
}

class GridEx(val row: Int, val col: Int) extends Iterable[CellEx] {
  protected val data: Vector[CellEx] = Vector.from(
    for {
      r <- 0 until row
      c <- 0 until col
    } yield new CellExImpl(r, c, this)
  )

  def apply(r: Int, c: Int): CellEx = data(r * col + c)

  def isValid(cell: CellEx): Boolean = {
    if (cell.row < this.row && cell.col < this.col) this(cell.row, cell.col) == cell
    else false
  }

  def randomCell(r: Random): CellEx = data(r.nextInt(data.size))

  def adjacencyOf(cell: CellEx, direction: Direction): Option[CellEx] = {
    val r = cell.row; val c = cell.col
    val ret: Option[CellEx] = direction match {
      case NorthDir => if (r-1 >= 0) Some(this(r-1, c)) else None
      case SouthDir => if (r+1 < row) Some(this(r+1, c)) else None
      case WestDir => if (c-1 >= 0) Some(this(r, c-1)) else None
      case EastDir => if (c+1 < col) Some(this(r, c+1)) else None
      case _ => None
    }
    ret
  }

  override def iterator: Iterator[CellEx] = new Iterator[CellEx] {
    private var _row = 0;
    private var _col = 0;

    override def hasNext: Boolean = _row < GridEx.this.row && _col < GridEx.this.col

    override def next(): CellEx = {
      val c = GridEx.this(_row, _col)
      forward()
      c
    }

    private def forward(): Unit = {
      if (_col+1 < GridEx.this.col) {
        _col += 1
      } else {
        _col = 0
        _row += 1
      }
    }
  }
}
