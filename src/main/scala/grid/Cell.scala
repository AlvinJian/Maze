package grid

trait Cell2D {
  type T <: Cell2D
  val row: Int
  val col: Int
  def neighbors: List[T]
}

trait Cell2DCart extends Cell2D {
  override type T = Cell2DCart

  def north: Option[T]
  def south: Option[T]
  def east: Option[T]
  def west: Option[T]
}

trait Cell2DPolar extends Cell2D {
  type T = Cell2DPolar
  // TODO
}
