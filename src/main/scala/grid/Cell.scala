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
  def cw: T
  def ccw: T
  // TODO for a polar cell, its outward neighbor can be 2. Fix it!!!
  def outward: List[T]
  def inward: Option[T]
  override def neighbors: List[Cell2DPolar] = List(cw, ccw) ++ outward ++ {
    if (inward.isDefined) List(inward.get) else Nil
  }
}
