package grid
import scala.util.Random

case class GridEx(override val rows: Int,
                  override val cols: Int) extends CellContainer[Cell2DCart] {
  protected val data: Vector[Cell2DCart] = Vector.from(
    for {
      r <- 0 until rows
      c <- 0 until cols
    } yield new GridExCell(r, c)
  )

  def apply(r: Int, c: Int): Cell2DCart = data(r * cols + c)

  def iterator: Iterator[Cell2DCart] = data.iterator

  private class GridExCell(override val row: Int, override val col: Int) extends Cell2DCart {
    override type T = Cell2DCart
    val outer: GridEx = GridEx.this

    override def north: Option[Cell2DCart] = {
      val (r, c) = (row-1, col)
      if (outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def south: Option[Cell2DCart] = {
      val (r, c) = (row+1, col)
      if (outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def east: Option[Cell2DCart] = {
      val (r, c) = (row, col+1)
      if (outer.isValid(r, c)) Some(outer(r, c)) else None
    }

    override def west: Option[Cell2DCart] = {
      val (r, c) = (row, col-1)
      if (outer.isValid(r, c)) Some(outer(r, c)) else None
    }
  }

  override def randomCell(r: Random): Cell2DCart = data(r.nextInt(data.size))
}
