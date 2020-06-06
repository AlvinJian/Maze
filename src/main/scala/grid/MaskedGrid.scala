package grid
import scala.util.{Random, Try}

case class MaskedGrid(override val rows: Int, override val cols: Int,
                      blacklist: Set[(Int,Int)]) extends GridContainer[Cell2DCart] {
  override protected val data: Vector[Cell2DCart] = Vector.from(
    for {
      r <- 0 until rows
      c <- 0 until cols
    } yield {
      val pos = (r, c);
      new MaskedGridCell(r, c, blacklist.contains(pos))
    }
  )

  override def apply(r: Int, c: Int): Cell2DCart = data(r * cols + c)

  override def isValid(cell: Cell2D): Boolean = if (super.isValid(cell) &&
    !cell.asInstanceOf[MaskedGridCell].masked) true else false

  // if the adjacent cell is masked, return None
  override def adjacencyOf(cell: Cell2D, direction: Direction): Option[Cell2DCart] = {
    var ret = super.adjacencyOf(cell, direction)
    if (ret.isDefined && !isValid(ret.get)) ret = None
    ret
  }

  override def randomCell(r: Random): Cell2DCart = {
    var cell = this(0, 0)
    do {
      cell = super.randomCell(r)
    } while (!isValid(cell))
    cell
  }

  override def iterator: Iterator[Cell2DCart] = new Iterator[Cell2DCart] {
    private val outer: MaskedGrid = MaskedGrid.this
    private var _row = 0;
    private var _col = 0;
    private var buffer: Option[Cell2DCart] = Some(MaskedGrid.this(_row, _col))
    bypassInvalid()

    override def hasNext: Boolean = buffer.isDefined

    override def next(): Cell2DCart = {
      val ret: Cell2DCart = buffer.get
      forward(); bypassInvalid()
      ret
    }

    private def bypassInvalid(): Unit = {
      while (_row < outer.rows && _col < outer.cols &&
        !outer.isValid(MaskedGrid.this(_row, _col))) forward()
      buffer = if (_row < outer.rows && _col < outer.cols)
        Some(outer(_row, _col)) else None
    }

    private def forward(): Unit = {
      if (_col+1 < outer.cols) {
        _col += 1
      } else {
        _col = 0
        _row += 1
      }
    }
  }

  class MaskedGridCell(override val row: Int, override val col: Int,
                       val masked: Boolean)
    extends Cell2DCart {
    val outer: MaskedGrid = MaskedGrid.this

    override def north: Option[Cell2DCart] =
      if (masked) None else outer.adjacencyOf(this, NorthDir)

    override def south: Option[Cell2DCart] =
      if (masked) None else outer.adjacencyOf(this, SouthDir)

    override def east: Option[Cell2DCart] =
      if (masked) None else outer.adjacencyOf(this, EastDir)

    override def west: Option[Cell2DCart] =
      if (masked) None else outer.adjacencyOf(this, WestDir)

    override def neighbors: List[Cell2DCart] =
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
