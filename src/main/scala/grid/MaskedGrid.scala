package grid
import scala.annotation.tailrec
import scala.util.{Random, Try}

case class MaskedGrid(override val rows: Int, override val cols: Int,
                      blacklist: Set[(Int,Int)]) extends CellContainer[Cell2DCart] {
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

  override def isValid(cell: Cell2D): Boolean = super.isValid(cell) &&
    !cell.asInstanceOf[MaskedGridCell].masked

  override def isValid(r: Int, c: Int): Boolean = super.isValid(r, c) &&
    !this(r, c).asInstanceOf[MaskedGridCell].masked

  override def randomCell(r: Random): Cell2DCart = {
    var cell = this(0, 0)
    do {
      cell = super.randomCell(r)
    } while (!isValid(cell))
    cell
  }

  override def iterator: Iterator[Cell2DCart] = new Iterator[Cell2DCart] {
    private val outer: MaskedGrid = MaskedGrid.this
    private val iter = outer.data.iterator
    private var cache: Option[Cell2DCart] = getNextValid

    override def hasNext: Boolean = cache.isDefined

    override def next(): Cell2DCart = {
      val ret: Cell2DCart = cache.get
      cache = getNextValid
      ret
    }

    @tailrec
    private def getNextValid: Option[Cell2DCart] = {
      if (iter.hasNext) {
        val cell = iter.next()
        if (outer.isValid(cell)) Some(cell)
        else getNextValid
      } else None
    }
  }

  class MaskedGridCell(override val row: Int, override val col: Int,
                       val masked: Boolean) extends Cell2DCart {
    override type T = Cell2DCart
    val outer: MaskedGrid = MaskedGrid.this

    override def north: Option[Cell2DCart] = {
      val (r, c) = (row-1, col)
      if (!masked && outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def south: Option[Cell2DCart] = {
      val (r, c) = (row+1, col)
      if (!masked && outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def east: Option[Cell2DCart] = {
      val (r, c) = (row, col+1)
      if (!masked && outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def west: Option[Cell2DCart] = {
      val (r, c) = (row, col-1)
      if (!masked && outer.isValid(r, c)) Some(outer(r, c)) else None
    }
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
