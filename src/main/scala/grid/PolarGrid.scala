package grid

import scala.annotation.tailrec

case class PolarGrid(override val rows: Int) extends CellContainer[Cell2DPolar] {
  override val cols: Int = 1// dirty hack

  protected val (data: Vector[Cell2DPolar], offsets: Vector[Int]) = {
    val rowHeight: Double = 1.0
    @tailrec
    def buildData(r: Int, tmpData: Vector[Cell2DPolar],
                  offsets: Vector[Int], prevCount: Int): (Vector[Cell2DPolar], Vector[Int]) = {
      if (r == 0) {
        buildData(r+1, tmpData :+ new PolarCellImp(0,0), offsets :+ 0, 1)
      } else if (r < this.rows) {
        val radius = r.toDouble
        val circum = 2.0 * math.Pi * radius
        val estCellWidth = circum / prevCount.toDouble
        val ratio = (estCellWidth / rowHeight).round.toInt
        val newCellCount = prevCount * ratio
        val newCells = for (col <- 0 until newCellCount) yield new PolarCellImp(r, col)
        buildData(r+1, tmpData ++ Vector.from(newCells), offsets :+ tmpData.size, newCellCount)
      } else (tmpData, offsets :+ tmpData.size)
    }
    buildData(0, Vector[Cell2DPolar](), Vector[Int](), 0)
  }

  override def isValid(r: Int, c: Int): Boolean = (r >= 0 && r < rows)

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

  override def isValid(t: Cell2D): Boolean = t.row < this.rows && apply(t.row, t.col) == t

  def columnCountAt(r: Int): Int = this.offsets(r+1) - this.offsets(r)

  private class PolarCellImp(override val row: Int,
                             override val col: Int) extends Cell2DPolar {
    override type T = Cell2DPolar

    val outer = PolarGrid.this

    override def cw: Cell2DPolar = outer(row, col-1)

    override def ccw: Cell2DPolar = outer(row, col+1)

    override def outward: List[Cell2DPolar] = {
      // TODO this is just a hotfix
      if (row == 0) List.from(outer.data.filter((c)=>c.row==1))
      else if (row < outer.rows-1) {
        val rc = outer.columnCountAt(row)
        val outerRc = outer.columnCountAt(row+1)
        if (outerRc > rc) List(outer(row+1, col*2), outer(row+1, col*2+1))
        else List(outer(row+1, col))
      } else List[Cell2DPolar]()
    }

    override def inward: Option[Cell2DPolar] = {
      if (row == 0) None
      else if (row == 1) Some(outer(0, 0))
      else {
        val rc = outer.columnCountAt(row)
        val innerRc = outer.columnCountAt(row-1)
        if (rc != innerRc) Some(outer(row-1, col/2))
        else Some(outer(row-1, col))
      }
    }
  }

  override def iterator: Iterator[Cell2DPolar] = data.iterator
}
