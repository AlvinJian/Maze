package maze

import scala.util.Random

case class Position2D(row: Int, col: Int)

trait Cell2D {
  type T <: Cell2D
  def container: Maze[T]
  def pos: Position2D
  def neighbors: List[T]
  def link(other: T): Option[Maze[T]]
  def linkedCells: Set[T]
  def sameAs(other: T): Boolean = container == other.container && pos == other.pos
}

trait MazeDimension{}

trait Maze[T <: Cell2D] extends Iterable[T] {
  def at(position: Position2D): Option[T]
  def at(r: Int, c: Int): Option[T] = at(Position2D(r,c))
  def dimension: MazeDimension
  def randomCell(r: Random): T = {
    val arr = this.toSeq
    arr(r.nextInt(arr.length))
  }
}
