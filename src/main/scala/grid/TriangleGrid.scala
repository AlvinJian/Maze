package grid
import scala.util.Random

case class TriangleGrid(override val rows: Int,
                        override val cols: Int) extends CellContainer[Cell2DTriangle] {

  protected val data: Vector[Cell2DTriangle] = Vector.from {
    for {
      r <- 0.until(rows)
      c <- 0.until(cols)
    } yield new Cell2DTriangleImpl(r , c)
  }

  override def iterator: Iterator[Cell2DTriangle] = data.iterator

  private class Cell2DTriangleImpl(override val row: Int,
                                   override val col: Int) extends Cell2DTriangle {
    override type T = Cell2DTriangle

    val outer: TriangleGrid = TriangleGrid.this
    override def isUpright: Boolean = {
      val sum = row + col
      sum % 2 == 0
    }

    override def north: Option[Cell2DTriangle] = if (!isUpright) Cell2DCart.north(outer, row, col) else None

    override def south: Option[Cell2DTriangle] = if (isUpright) Cell2DCart.south(outer, row, col) else None

    override def east: Option[Cell2DTriangle] = Cell2DCart.east(outer, row, col)

    override def west: Option[Cell2DTriangle] = Cell2DCart.west(outer, row, col)
  }

  override def apply(r: Int, c: Int): Cell2DTriangle = data(r * cols + c)

  override def randomCell(r: Random): Cell2DTriangle = data(r.nextInt(data.size))
}
