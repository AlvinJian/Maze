package grid

case class HexGrid(override val rows: Int,
                   override val cols: Int) extends CellContainer[Cell2DHex] {
  override protected val data: Vector[Cell2DHex] = Vector.from{
    for {
      r <- 0 until rows
      c <- 0 until cols
    } yield new HexCellImpl(r, c)
  }

  override def apply(r: Int, c: Int): Cell2DHex = data(r * cols + c)

  override def iterator: Iterator[Cell2DHex] = data.iterator

  private class HexCellImpl(override val row: Int,
                            override val col: Int) extends Cell2DHex {
    val outer: HexGrid = HexGrid.this

    private def northDiagonalRow: Int = if (col % 2  == 0) row - 1 else row

    private def southDiagonalRow: Int = if (col % 2 == 0) row else row + 1

    override def northeast: Option[Cell2DHex] = {
      val r = northDiagonalRow
      val c = col + 1
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }

    override def north: Option[Cell2DHex] = {
      val r = row-1
      val c = col
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }

    override def northwest: Option[Cell2DHex] = {
      val (r, c) = (northDiagonalRow, col-1)
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }

    override def southwest: Option[Cell2DHex] = {
      val (r ,c) = (southDiagonalRow, col-1)
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }

    override def south: Option[Cell2DHex] = {
      val (r , c) = (row+1, col)
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }

    override def southeast: Option[Cell2DHex] = {
      val (r, c) = (southDiagonalRow, col+1)
      if (outer.isValid(r, c)) Some(outer.apply(r, c)) else None
    }
  }
}
