package maze

trait PlainGrid[T <: Position2D] extends Iterable[T] {
  def at(r: Int, c: Int): Option[T]
  def isValid(pos: Position2D): Boolean
}
