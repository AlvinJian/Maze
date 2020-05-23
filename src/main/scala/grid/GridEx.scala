package grid

import scala.util.Random

case class GridEx(override val row: Int,
                  override val col: Int) extends GridContainer[CellEx] {

  protected val data: Vector[CellEx] = Vector.from(
    for {
      r <- 0 until row
      c <- 0 until col
    } yield new GridExCell(r, c)
  )

  def apply(r: Int, c: Int): CellEx = data(r * col + c)

  def iterator: Iterator[CellEx] = new Iterator[CellEx] {
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

  private class GridExCell(override val row: Int, override val col: Int) extends CellEx {
    val outer: GridEx = GridEx.this

    override def north: Option[CellEx] = outer.adjacencyOf(this, NorthDir)

    override def south: Option[CellEx] = outer.adjacencyOf(this, SouthDir)

    override def east: Option[CellEx] = outer.adjacencyOf(this, EastDir)

    override def west: Option[CellEx] = outer.adjacencyOf(this, WestDir)

    override def neighbors: List[CellEx] = List(this.north, this.south, this.east, this.west).flatten
  }
}
