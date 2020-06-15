package grid

import scala.util.Random

trait CellContainer[+T <: Cell2D] extends Iterable[T] {
  val rows: Int
  val cols: Int

  def apply(r: Int, c: Int): T

  def randomCell(r: Random): T = data(r.nextInt(data.size))

  def isValid(t: Cell2D): Boolean =
    t.row < this.rows && t.col < this.cols && apply(t.row, t.col) == t

  def isValid(r: Int, c: Int): Boolean = r < rows && r >= 0 && c < cols && c >= 0

  // default implementation for cartesian 2d cell
  // should be overridden if using other coordinate system
  @deprecated
  def adjacencyOf(t: Cell2D, direction: Direction): Option[T] = {
    if (!isValid(t)) return None
    val r = t.row; val c = t.col
    val ret: Option[T] = direction match {
      case NorthDir => if (r-1 >= 0) Some(this(r-1, c)) else None
      case SouthDir => if (r+1 < rows) Some(this(r+1, c)) else None
      case WestDir => if (c-1 >= 0) Some(this(r, c-1)) else None
      case EastDir => if (c+1 < cols) Some(this(r, c+1)) else None
      case _ => None
    }
    ret
  }

  // TODO no longer need to enforce data member here
  protected val data: Vector[T]
}
