package maze

import scala.util.Random

/*
 * Cell trait is stateful, depending on Maze[T]; Position is not.
 */
/*
 * TODO need more flexible design in Position type, ex. trait Position
 * to fix weave maze
 */
case class Position2D(row: Int, col: Int)
object Position2D {
  implicit val ordering: Ordering[Position2D] = (x: Position2D, y: Position2D) => {
    if (x.row == y.row) x.col - y.col
    else x.row - y.row
  }
}

trait Cell2D {
  type T <: Cell2D
  def container: Maze[T]
  def pos: Position2D
  def neighbors: List[T]
  def linked: List[T] = container.linkedBy(this.pos)
}

trait Maze[+T <: Cell2D] extends Iterable[T] {
  def at(position: Position2D): Option[T]
  def at(r: Int, c: Int): Option[T] = at(Position2D(r,c))
  def neighborsAt(pos: Position2D): List[T] = at(pos).map(c=>c.neighbors)
    .fold(List[T]())(cells => cells.map(c => c.asInstanceOf[T]))
  def linkedBy(position: Position2D): List[T]
  def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]]
  def randomCell(r: Random): T = {
    val arr = this.toSeq
    arr(r.nextInt(arr.length))
  }
  def link(pos1: Position2D, pos2: Position2D): Option[Maze[T]]

  def neighborsAt(cell: Cell2D): List[T] = {
    if (cell.container != this) return Nil
    neighborsAt(cell.pos)
  }
  def linkedBy(cell: Cell2D): List[T] = {
    if (cell.container != this) return Nil
    linkedBy(cell.pos)
  }
}
