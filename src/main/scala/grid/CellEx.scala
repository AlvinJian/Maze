package grid

trait CellPosition {
  val row: Int
  val col: Int
}

trait CellEx extends CellPosition {
  def north: Option[CellEx]
  def south: Option[CellEx]
  def east: Option[CellEx]
  def west: Option[CellEx]
  def neighbors: List[CellEx]
}