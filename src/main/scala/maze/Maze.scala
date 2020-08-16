package maze

import scala.util.Random

/*
 * Cell trait is stateful, depending on Maze[T]; Position is not.
 */

case class Position2D(row: Int, col: Int)

trait Cell2D {
  type T <: Cell2D
  def container: Maze[T]
  def pos: Position2D
  def neighbors: List[T]
  def linked: Set[T]
}

trait Maze[+T <: Cell2D] extends Iterable[T] {
  def at(position: Position2D): Option[T]
  def at(r: Int, c: Int): Option[T] = at(Position2D(r,c))
  def neighborsAt(pos: Position2D): List[T] = {
    at(pos).map(c=>c.neighbors) match {
      case Some(value) => value.map(c => c.asInstanceOf[T])
      case None => Nil
    }
  }
  def info: MazeInfo
  def randomCell(r: Random): T = {
    val arr = this.toSeq
    arr(r.nextInt(arr.length))
  }
  def link(pos1: Position2D, pos2: Position2D): Option[Maze[T]]
  def linkedPositions(pos: Position2D): Set[Position2D]
}
