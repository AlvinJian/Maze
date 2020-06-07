package grid

case class GridEx(override val rows: Int,
                  override val cols: Int) extends CellContainer[Cell2DCart] {
  protected val data: Vector[Cell2DCart] = Vector.from(
    for {
      r <- 0 until rows
      c <- 0 until cols
    } yield new GridExCell(r, c)
  )

  def apply(r: Int, c: Int): Cell2DCart = data(r * cols + c)

  def iterator: Iterator[Cell2DCart] = new Iterator[Cell2DCart] {
    private val outer: GridEx = GridEx.this
    private var _row = 0;
    private var _col = 0;

    override def hasNext: Boolean = _row < outer.rows && _col < outer.cols

    override def next(): Cell2DCart = {
      val c = outer(_row, _col)
      forward()
      c
    }

    private def forward(): Unit = {
      if (_col+1 < outer.cols) {
        _col += 1
      } else {
        _col = 0; _row += 1
      }
    }
  }

  private class GridExCell(override val row: Int, override val col: Int) extends Cell2DCart {
    val outer: GridEx = GridEx.this

    override def north: Option[Cell2DCart] = outer.adjacencyOf(this, NorthDir)

    override def south: Option[Cell2DCart] = outer.adjacencyOf(this, SouthDir)

    override def east: Option[Cell2DCart] = outer.adjacencyOf(this, EastDir)

    override def west: Option[Cell2DCart] = outer.adjacencyOf(this, WestDir)
  }
}
