package grid
import scala.util.{Random, Try}

case class MaskedGrid(override val row: Int, override val col: Int,
                 blacklist: Set[(Int,Int)]) extends GridContainer[CellEx] {
  override protected val data: Vector[CellEx] = Vector.from(
    for {
      r <- 0 until row
      c <- 0 until col
    } yield {
      val pos = (r, c);
      new MaskedGridCell(r, c, blacklist.contains(pos))
    }
  )

  override def apply(r: Int, c: Int): CellEx = data(r * col + c)

  override def isValid(cell: CellEx): Boolean = if (super.isValid(cell) &&
    !cell.asInstanceOf[MaskedGridCell].masked) true else false

  // if the adjacent cell is masked, return None
  override def adjacencyOf(cell: CellEx, direction: Direction): Option[CellEx] = {
    var ret = super.adjacencyOf(cell, direction)
    if (ret.isDefined && !isValid(ret.get)) ret = None
    ret
  }

  override def randomCell(r: Random): CellEx = {
    var cell = this(0, 0)
    do {
      cell = super.randomCell(r)
    } while (!isValid(cell))
    cell
  }

  override def iterator: Iterator[CellEx] = new Iterator[CellEx] {
    private var _row = 0;
    private var _col = 0;
    private var buffer: Option[CellEx] = Some(MaskedGrid.this(_row, _col))
    bypassInvalid()

    override def hasNext: Boolean = buffer.isDefined

    override def next(): CellEx = {
      val ret: CellEx = buffer.get
      forward(); bypassInvalid()
      ret
    }

    private def bypassInvalid(): Unit = {
      while (_row < MaskedGrid.this.row &&
        _col < MaskedGrid.this.col &&
        !MaskedGrid.this.isValid(MaskedGrid.this(_row, _col))) forward()
      buffer = if (_row < MaskedGrid.this.row && _col < MaskedGrid.this.col)
        Some(MaskedGrid.this(_row, _col)) else None
    }

    private def forward(): Unit = {
      if (_col+1 < MaskedGrid.this.col) {
        _col += 1
      } else {
        _col = 0
        _row += 1
      }
    }
  }

  class MaskedGridCell(override val row: Int, override val col: Int,
                       val masked: Boolean)
    extends CellEx {
    val outer: MaskedGrid = MaskedGrid.this

    override def north: Option[CellEx] =
      if (masked) None else outer.adjacencyOf(this, NorthDir)

    override def south: Option[CellEx] =
      if (masked) None else outer.adjacencyOf(this, SouthDir)

    override def east: Option[CellEx] =
      if (masked) None else outer.adjacencyOf(this, EastDir)

    override def west: Option[CellEx] =
      if (masked) None else outer.adjacencyOf(this, WestDir)

    override def neighbors: List[CellEx] =
      List(this.north, this.south, this.east, this.west).flatten
  }
}

object MaskedGrid {
  def from(strMask: String): MaskedGrid = {
    var row = 0;
    var col, c = 0
    var blacklist = Set[(Int,Int)]()
    for (ch <- strMask) {
      if (ch.toUpper == 'O' || ch == '.') c+=1
      else if (ch.toUpper == 'X') {
        blacklist = blacklist + ((row, c)); c+=1
      }
      else if (ch == '\n') {
        col = math.max(c, col)
        c = 0; row += 1
      }
    }
    row = if (c > 0) row+1 else row
    new MaskedGrid(row, col, blacklist)
  }
}
