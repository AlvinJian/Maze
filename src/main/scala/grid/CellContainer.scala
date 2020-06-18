package grid

import scala.util.Random

trait CellContainer[+T <: Cell2D] extends Iterable[T] {
  val rows: Int
  val cols: Int

  def apply(r: Int, c: Int): T

  def randomCell(r: Random): T = data(r.nextInt(data.size))

  def isValid(t: Cell2D): Boolean =
    isValid(t.row, t.col) && apply(t.row, t.col) == t

  def isValid(r: Int, c: Int): Boolean = r < rows && r >= 0 && c < cols && c >= 0

  // TODO no longer need to enforce data member here
  protected val data: Vector[T]
}
