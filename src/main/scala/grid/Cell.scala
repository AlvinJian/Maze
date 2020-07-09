package grid

trait Cell2D {
  type T <: Cell2D
  val row: Int
  val col: Int
  def neighbors: List[T]
}

trait Cell2DCart extends Cell2D {
  override type T <: Cell2DCart
  def north: Option[T]
  def south: Option[T]
  def east: Option[T]
  def west: Option[T]
  override def neighbors: List[T] = List(this.north, this.south, this.east, this.west).flatten
}

trait Cell2DPolar extends Cell2D {
  override type T <: Cell2DPolar
  def cw: T
  def ccw: T
  def outward: List[T] // can have more than one cells
  def inward: Option[T] // either one cell or none
  override def neighbors: List[T] = {
    val candidates = List(cw, ccw) ++ outward ++ {
      if (inward.isDefined) List(inward.get) else Nil
    }
    candidates.filter(c => c != this)
  }
}

trait Cell2DHex extends Cell2D {
  override type T <: Cell2DHex
  def northeast: Option[T]
  def north: Option[T]
  def northwest: Option[T]
  def southwest: Option[T]
  def south: Option[T]
  def southeast: Option[T]
  override def neighbors: List[T] = List(
    northeast, north, northwest, southwest, south, southeast
  ).flatten
}

trait Cell2DTriangle extends Cell2DCart {
  override type T <: Cell2DTriangle
  def isUpright: Boolean
}

trait Cell2DWeave extends Cell2DCart {
  override type T <: Cell2DWeave
}

trait Cell2DOverlay extends Cell2DWeave {
  def underneath: Option[Cell2DHidden]
}

trait Cell2DHidden extends Cell2DCart {
  val overlay: Cell2DOverlay
  def isHorizontalLink: Boolean
  def isVerticalLink: Boolean
}
