package grid

import scala.annotation.tailrec

case class PolarGrid(override val rows: Int) extends CellContainer[Cell2DPolar] {
  override val cols: Int = 1

  protected val (data: Vector[Cell2DPolar], offsets: Vector[Int]) = {
    val rowHeight: Double = 1.0 / rows.toDouble
    @tailrec
    def buildData(r: Int, tmpData: Vector[Cell2DPolar],
                  offsets: Vector[Int], prevCount: Int): (Vector[Cell2DPolar], Vector[Int]) = {
      if (r == 0) {
        buildData(r+1, tmpData :+ new PolarCellImp(0,0), offsets :+ 0, 1)
      } else if (r < this.rows) {
        val radius = r.toDouble / this.rows.toDouble
        val circum = 2.0 * math.Pi * radius
        val estCellWidth = circum / prevCount.toDouble
        val ratio = estCellWidth / rowHeight

        val newCellCount = (prevCount * ratio).round.toInt
        val newCells = for (col <- 0 until newCellCount) yield new PolarCellImp(r, col)
        buildData(r+1, tmpData ++ Vector.from(newCells), offsets :+ tmpData.size, newCellCount)
      } else (tmpData, offsets :+ tmpData.size)
    }
    buildData(0, Vector[Cell2DPolar](), Vector[Int](), 0)
  }

  override def apply(r: Int, c: Int): Cell2DPolar = {
    val offset = this.offsets(r)
    val colCount = columnCountAt(r)
    @tailrec
    def calcIndex(num: Int): Int = {
      if (num >= 0) num % colCount
      else calcIndex(num + colCount)
    }
    val index = calcIndex(c)
    data(offset + index)
  }

  override def isValid(t: Cell2D): Boolean = {
    if (t.row < this.rows && apply(t.row, t.col) == t) true
    else false
  }

  override def adjacencyOf(t: Cell2D, direction: Direction): Option[Cell2DPolar] = {
    if (isValid(t)) {
      val r = t.row;
      val c = t.col;
      direction match {
        case ClockwiseDir => Some(this(r, c-1))
        case CounterClockwiseDir => Some(this(r, c+1))
        case InwardDir => if (r-1 >= 0) Some(this(r-1, c)) else None
        case OutwardDir => if (r+1 < this.rows) Some(this(r+1, c)) else None
        case _ => None
      }
    } else None
  }

  def columnCountAt(r: Int): Int = this.offsets(r+1) - this.offsets(r)

  private class PolarCellImp(override val row: Int,
                             override val col: Int) extends Cell2DPolar {
    val outer = PolarGrid.this
    override def cw: Option[Cell2DPolar] = outer.adjacencyOf(this, ClockwiseDir)

    override def ccw: Option[Cell2DPolar] = outer.adjacencyOf(this, CounterClockwiseDir)

    override def outward: Option[Cell2DPolar] = outer.adjacencyOf(this, OutwardDir)

    override def inward: Option[Cell2DPolar] = outer.adjacencyOf(this,InwardDir)
  }

  override def iterator: Iterator[Cell2DPolar] = data.iterator
}
