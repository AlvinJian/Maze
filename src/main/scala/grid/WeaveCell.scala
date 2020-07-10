package grid

class Cell2DOverlay(val outer: WeaveGrid,
                    override val row: Int,
                    override val col: Int) extends Cell2DWeave {
  override type T = Cell2DWeave

  def canTunnelNorth: Boolean =
    north.isDefined && north.get.north.isDefined && north.get.isHorizontalLinked

  def canTunnelSouth: Boolean =
    south.isDefined && south.get.south.isDefined && south.get.isHorizontalLinked

  def canTunnelEast: Boolean =
    east.isDefined && east.get.east.isDefined && east.get.isVerticalLinked

  def canTunnelWest: Boolean =
    west.isDefined && west.get.west.isDefined && west.get.isVerticalLinked

  override def isHorizontalLinked: Boolean = {
    if (east.isDefined && west.isDefined) {
      outer.isLinked(this, east.get) &&
        outer.isLinked(this, west.get) && {
        north.isEmpty || !outer.isLinked(this, north.get)
      } && {
        south.isEmpty || !outer.isLinked(this, south.get)
      }
    } else false
  }

  override def isVerticalLinked: Boolean = {
    if (north.isDefined && south.isDefined) {
      outer.isLinked(this, north.get) &&
        outer.isLinked(this, south.get) && {
        east.isEmpty || !outer.isLinked(this, east.get)
      } && {
        west.isEmpty || !outer.isLinked(this, west.get)
      }
    } else false
  }

  def underneath: Option[Cell2DHidden] = outer.hiddenCell(row, col)

  override def neighbors: List[Cell2DWeave] = {
    var neighbors = super.neighbors
    neighbors = neighbors ++ {
      if (canTunnelNorth) List(north.get.north).flatten else Nil
    }
    neighbors = neighbors ++ {
      if (canTunnelSouth) List(south.get.south).flatten else Nil
    }
    neighbors = neighbors ++ {
      if (canTunnelEast) List(east.get.east).flatten else Nil
    }
    neighbors = neighbors ++ {
      if (canTunnelWest) List(west.get.west).flatten else Nil
    }
    neighbors
  }

  override def north: Option[Cell2DWeave] = {
    val optWeaveCell = Cell2DCart.north(outer, row, col)
    if (optWeaveCell.isDefined) {
      val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
      if (overlay.underneath.isDefined && overlay.isHorizontalLinked) overlay.underneath
      else optWeaveCell
    } else optWeaveCell
  }

  override def south: Option[Cell2DWeave] = {
    val optWeaveCell = Cell2DCart.south(outer, row, col)
    if (optWeaveCell.isDefined) {
      val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
      if (overlay.underneath.isDefined && overlay.isHorizontalLinked) overlay.underneath
      else optWeaveCell
    } else optWeaveCell
  }

  override def east: Option[Cell2DWeave] = {
    val optWeaveCell = Cell2DCart.east(outer, row, col)
    if (optWeaveCell.isDefined) {
      val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
      if (overlay.isVerticalLinked && overlay.underneath.isDefined) overlay.underneath
      else optWeaveCell
    } else optWeaveCell
  }

  override def west: Option[Cell2DWeave] = {
    val optWeaveCell = Cell2DCart.west(outer, row, col)
    if (optWeaveCell.isDefined) {
      val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
      if (overlay.isVerticalLinked && overlay.underneath.isDefined) overlay.underneath
      else optWeaveCell
    } else optWeaveCell
  }
}

class Cell2DHidden(val overlay: Cell2DOverlay) extends Cell2DWeave {
  override type T = Cell2DWeave

  val outer: WeaveGrid = overlay.outer
  override val row: Int = overlay.row
  override val col: Int = overlay.col

  override def north: Option[Cell2DWeave] = {
    if (overlay.isHorizontalLinked) Cell2DCart.north(outer, row, col)
    else None
  }

  override def south: Option[Cell2DWeave] = {
    if (overlay.isHorizontalLinked) Cell2DCart.south(outer, row, col)
    else None
  }

  override def east: Option[Cell2DWeave] = {
    if (overlay.isVerticalLinked) Cell2DCart.east(outer, row, col)
    else None
  }

  override def west: Option[Cell2DWeave] = {
    if (overlay.isVerticalLinked) Cell2DCart.west(outer, row, col)
    else None
  }

  override def isHorizontalLinked: Boolean = east.isDefined || west.isDefined

  override def isVerticalLinked: Boolean = north.isDefined || south.isDefined
}
