package maze

import scala.util.Random

trait Cell2DWeave extends Cell2DRect {
  override type T <: Cell2DWeave
  def isHorizontalLinked: Boolean
  def isVerticalLinked: Boolean
  def isHidden: Boolean
}

private[maze] class WeaveMaze(val grid: RectGrid, val graph: Graph,
                              hidden: List[Position2D], val linkedToHidden: Graph) extends Maze[Cell2DWeave] {
  private val helper = new MazeHelper[Cell2DWeave, RectGrid](p => Cell2DOverlay(p), grid, graph)

  def this(rows: Int, cols: Int) = this(RectGrid(rows, cols), new Graph(), Nil, new Graph())

  override def at(position: Position2D): Option[Cell2DWeave] = helper.cells.get(position)

  override def linkedBy(position: Position2D): List[Cell2DWeave] = {
    helper.linkedBy(position) ++ {
      linkedToHidden.linked(position).map(p => hiddenCells(p))
    }
  }

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = WeaveMazeInfo(grid, this)

//  override def randomCell(r: Random): Cell2DWeave = {
//    val overlayCells = helper.cells.values.toArray
//    val id = r.nextInt(overlayCells.size)
//    overlayCells(id)
//  }

  private def checkHiddenCellRequired(pos1: Position2D,
                                      pos2: Position2D): Option[Cell2DOverlay] = {
    if (!grid.isValid(pos1) || !grid.isValid(pos2)) return None
    val c1 = helper.cells(pos1).asInstanceOf[Cell2DOverlay]
    val c2 = helper.cells(pos2).asInstanceOf[Cell2DOverlay]
    val tmp: Option[Cell2DWeave] = {
      if (c1.north.isDefined && c1.north == c2.south) c1.north
      else if (c1.south.isDefined && c1.south == c2.north) c1.south
      else if (c1.west.isDefined && c1.west == c2.east) c1.west
      else if (c1.east.isDefined && c1.east == c2.west) c1.east
      else None
    }
    tmp.flatMap(c => c match {
      case overlay: Cell2DOverlay => Some(overlay)
      case _ => None
    }).filter(c => c.isHorizontalLinked || c.isVerticalLinked)
  }

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DWeave]] = {
    val optCell = checkHiddenCellRequired(pos1, pos2)
    optCell match {
      case Some(cell) =>  {
        val hg = linkedToHidden.dirLink(pos1, cell.pos).dirLink(pos2, cell.pos)
        Some(new WeaveMaze(grid, graph, hiddenCells.keys.toList :+ cell.pos, hg))
      }
      case None => {
        helper.link(pos1, pos2).map(g => new WeaveMaze(grid, g, hiddenCells.keys.toList, linkedToHidden))
      }
    }
  }

  override def iterator: Iterator[Cell2DWeave] = {
    val all = helper.cells.values.toList.sortBy(c => c.pos) ++ hiddenCells.values.toList.sortBy(c => c.pos)
    all.iterator
  }

  val hiddenCells: Map[Position2D, Cell2DHidden] = hidden.map(p => {
    val h = Cell2DHidden(helper.cells(p).asInstanceOf[Cell2DOverlay])
    (p, h)
  }).toMap

  case class Cell2DOverlay(pos: Position2D) extends Cell2DWeave {
    override type T = Cell2DWeave

    override def isHidden: Boolean = false

    override def container: Maze[Cell2DWeave] = WeaveMaze.this

//    override def pos: Position2D = p

    def canTunnelNorth: Boolean =
      north.isDefined && north.get.north.isDefined && {
        north.get.isHidden || north.get.isHorizontalLinked
      }

    def canTunnelSouth: Boolean =
      south.isDefined && south.get.south.isDefined && {
        south.get.isHidden || south.get.isHorizontalLinked
      }

    def canTunnelEast: Boolean =
      east.isDefined && east.get.east.isDefined && {
        east.get.isHidden || east.get.isVerticalLinked
      }

    def canTunnelWest: Boolean =
      west.isDefined && west.get.west.isDefined && {
        west.get.isHidden || west.get.isVerticalLinked
      }

    override def isHorizontalLinked: Boolean = {
      val _east = super.east
      val _west = super.west
      val _north = super.north
      val _south = super.south
      // TODO need cleanup
      if (_east.isDefined && _west.isDefined) {
        linked.contains(_east.get) &&
          linked.contains(_west.get) && {
          _north.isEmpty || !linked.contains(_north.get)
        } && {
          _south.isEmpty || !linked.contains(_south.get)
        }
      } else false
    }

    override def isVerticalLinked: Boolean = {
      val _east = super.east
      val _west = super.west
      val _north = super.north
      val _south = super.south
      // TODO need cleanup
      if (_north.isDefined && _south.isDefined) {
        linked.contains(_north.get) &&
          linked.contains(_south.get) && {
          _east.isEmpty || !linked.contains(_east.get)
        } && {
          _west.isEmpty || !linked.contains(_west.get)
        }
      } else false
    }

    def underneath: Option[Cell2DHidden] = WeaveMaze.this.hiddenCells.get(pos)

    override def north: Option[Cell2DWeave] = {
      val optWeaveCell = super.north
      if (optWeaveCell.isDefined) {
        val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
        if (overlay.underneath.isDefined && overlay.isHorizontalLinked) overlay.underneath
        else optWeaveCell
      } else optWeaveCell
    }

    override def south: Option[Cell2DWeave] = {
      val optWeaveCell = super.south
      if (optWeaveCell.isDefined) {
        val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
        if (overlay.underneath.isDefined && overlay.isHorizontalLinked) overlay.underneath
        else optWeaveCell
      } else optWeaveCell
    }

    override def east: Option[Cell2DWeave] = {
      val optWeaveCell = super.east
      if (optWeaveCell.isDefined) {
        val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
        if (overlay.isVerticalLinked && overlay.underneath.isDefined) overlay.underneath
        else optWeaveCell
      } else optWeaveCell
    }

    override def west: Option[Cell2DWeave] = {
      val optWeaveCell = super.west
      if (optWeaveCell.isDefined) {
        val overlay = optWeaveCell.get.asInstanceOf[Cell2DOverlay]
        if (overlay.isVerticalLinked && overlay.underneath.isDefined) overlay.underneath
        else optWeaveCell
      } else optWeaveCell
    }

    override def neighbors: List[Cell2DWeave] = {
      var neighbors = super.neighbors
      neighbors = neighbors ++ {
        if (canTunnelNorth) List(north.get.north.get) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelSouth) List(south.get.south.get) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelEast) List(east.get.east.get) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelWest) List(west.get.west.get) else Nil
      }
      neighbors.filter(c => true/*!c.isHidden*/)
    }
  }

  case class Cell2DHidden(overlay: Cell2DOverlay) extends Cell2DWeave {
    override type T = Cell2DWeave

    override def isHorizontalLinked: Boolean = east.isDefined || west.isDefined

    override def isVerticalLinked: Boolean = north.isDefined || south.isDefined

    override def isHidden: Boolean = true

    override def container: Maze[Cell2DWeave] = WeaveMaze.this

    override def pos: Position2D = overlay.pos

    override def north: Option[Cell2DWeave] = {
      if (overlay.isHorizontalLinked) super.north else None
    }

    override def south: Option[Cell2DWeave] = {
      if (overlay.isHorizontalLinked) super.south else None
    }

    override def east: Option[Cell2DWeave] = {
      if (overlay.isVerticalLinked) super.east else None
    }

    override def west: Option[Cell2DWeave] = {
      if(overlay.isVerticalLinked) super.west else None
    }
  }
}

object WeaveMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DWeave] = new WeaveMaze(rows, cols)
}
