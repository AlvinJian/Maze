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
      north.fold(false)(n => n.north.isDefined) &&
        north.fold(false)(n => n.isHidden || n.isHorizontalLinked)

    def canTunnelSouth: Boolean =
      south.map(s => s.south.isDefined).fold(false)(b => b) && {
        south.fold(false)(s => s.isHidden || s.isHorizontalLinked)
      }

    def canTunnelEast: Boolean =
      east.fold(false)(e => e.east.isDefined) && {
        east.fold(false)(e => e.isHidden || e.isVerticalLinked)
      }

    def canTunnelWest: Boolean =
      west.fold(false)(w => w.west.isDefined) && {
        west.fold(false)(w => w.isHidden || w.isVerticalLinked)
      }

    override def isHorizontalLinked: Boolean = {
      val _east = super.east
      val _west = super.west
      val _north = super.north
      val _south = super.south

      _east.map(e => linked.contains(e)).fold(false)(b => b) &&
        _west.map(w => linked.contains(w)).fold(false)(b => b) &&
        _north.map(n => !linked.contains(n)).fold(true)(b => b) &&
        _south.map(s => !linked.contains(s)).fold(true)(b => b)
    }

    override def isVerticalLinked: Boolean = {
      val _east = super.east
      val _west = super.west
      val _north = super.north
      val _south = super.south

      _north.map(n => linked.contains(n)).fold(false)(b => b) &&
        _south.map(s => linked.contains(s)).fold(false)(b => b) &&
        _east.map(e => !linked.contains(e)).fold(true)(b => b) &&
        _west.map(w => !linked.contains(w)).fold(true)(b => b)
    }

    def underneath: Option[Cell2DHidden] = WeaveMaze.this.hiddenCells.get(pos)

    override def north: Option[Cell2DWeave] = {
      val optCell = super.north
      optCell.map(c => {
        val overlay = c.asInstanceOf[Cell2DOverlay]
        if (overlay.isHorizontalLinked) overlay.underneath.fold(c)(u => u)
        else c
      })
    }

    override def south: Option[Cell2DWeave] = {
      val optCell = super.south
      optCell.map(c => {
        val overlay = c.asInstanceOf[Cell2DOverlay]
        if (overlay.isHorizontalLinked) overlay.underneath.fold(c)(u => u)
        else c
      })
    }

    override def east: Option[Cell2DWeave] = {
      val optCell = super.east
      optCell.map {
        c => {
          val overlay = c.asInstanceOf[Cell2DOverlay]
          if (overlay.isVerticalLinked) overlay.underneath.fold(c)(u => u)
          else c
        }
      }
    }

    override def west: Option[Cell2DWeave] = {
      val optCell = super.west
      optCell.map {
        c => {
          val overlay = c.asInstanceOf[Cell2DOverlay]
          if (overlay.isVerticalLinked) overlay.underneath.fold(c)(u => u)
          else c
        }
      }
    }

    override def neighbors: List[Cell2DWeave] = {
      var neighbors = super.neighbors
      neighbors = neighbors ++ {
        if (canTunnelNorth) north.flatMap(n => n.north) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelSouth) south.flatMap(s => s.south) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelEast) east.flatMap(e => e.east) else Nil
      }
      neighbors = neighbors ++ {
        if (canTunnelWest) west.flatMap(w => w.west) else Nil
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
