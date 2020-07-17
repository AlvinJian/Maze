package grid
import scala.util.Random

case class RectGrid(override val rows: Int,
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
    val outer: RectGrid = RectGrid.this

    override def north: Option[Cell2DCart] = Cell2DCart.north(outer, row, col)

    override def south: Option[Cell2DCart] = Cell2DCart.south(outer, row, col)

    override def east: Option[Cell2DCart] = Cell2DCart.east(outer, row, col)

    override def west: Option[Cell2DCart] = Cell2DCart.west(outer, row, col)
  }

  override def randomCell(r: Random): Cell2DCart = data(r.nextInt(data.size))
}
