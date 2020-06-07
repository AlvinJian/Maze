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
  override def neighbors: List[T] = List(this.north, this.south, this.east, this.west).flatten
}

trait Cell2DPolar extends Cell2D {
  type T = Cell2DPolar
  def cw: Option[T]
  def ccw: Option[T]
  def outward: Option[T]
  def inward: Option[T]
  override def neighbors: List[Cell2DPolar] = List(this.cw, this.ccw, this.inward, this.outward).flatten
}
