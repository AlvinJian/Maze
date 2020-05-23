package grid

import scala.util.Random

trait GridContainer[T <: CellPosition] extends Iterable[T] {
  val row: Int
  val col: Int

  def apply(r: Int, c: Int): T

  def randomCell(r: Random): T = data(r.nextInt(data.size))

  def isValid(t: T): Boolean = {
    if (t.row < this.row && t.col < this.col) this(t.row, t.col) == t
    else false
  }

  def adjacencyOf(t: T, direction: Direction): Option[T] = {
    val r = t.row; val c = t.col
    val ret: Option[T] = direction match {
      case NorthDir => if (r-1 >= 0) Some(this(r-1, c)) else None
      case SouthDir => if (r+1 < row) Some(this(r+1, c)) else None
      case WestDir => if (c-1 >= 0) Some(this(r, c-1)) else None
      case EastDir => if (c+1 < col) Some(this(r, c+1)) else None
      case _ => None
    }
    ret
  }

  protected val data: Vector[T]
}
