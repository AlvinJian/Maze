package grid

case class TriangleGrid(override val rows: Int,
                        override val cols: Int) extends CellContainer[Cell2DTriangle] {

  override protected val data: Vector[Cell2DTriangle] = Vector.from {
    for {
      r <- 0.until(rows)
      c <- 0.until(cols)
    } yield new Cell2DTriangleImpl(r , c)
  }

  override def iterator: Iterator[Cell2DTriangle] = data.iterator

  private class Cell2DTriangleImpl(override val row: Int,
                                   override val col: Int) extends Cell2DTriangle {
    val outer: TriangleGrid = TriangleGrid.this
    override def isUpright: Boolean = {
      val sum = row + col
      sum % 2 == 0
    }

    override def north: Option[Cell2DTriangle] = {
      val (r, c) = (row-1, col)
      if (!isUpright && outer.isValid(r, c)) Some(outer.apply(r, c))
      else None
    }

    override def south: Option[Cell2DTriangle] = {
      val (r, c) = (row+1, col)
      if (isUpright && outer.isValid(r, c)) Some(outer.apply(r, c))
      else None
    }

    override def east: Option[Cell2DTriangle] = {
      val (r ,c) = (row, col+1)
      if (outer.isValid(r, c)) Some(outer.apply(r, c))
      else None
    }

    override def west: Option[Cell2DTriangle] = {
      val (r ,c) = (row, col-1)
      if (outer.isValid(r, c)) Some(outer.apply(r, c))
      else None
    }
  }

  override def apply(r: Int, c: Int): Cell2DTriangle = data(r * cols + c)
}
