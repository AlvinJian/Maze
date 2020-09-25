package maze

import scala.util.Random

trait Cell2DWeave extends Cell2DRect {
  override type T <: Cell2DWeave
  def isHorizontalLinked: Boolean
  def isVerticalLinked: Boolean
  def isHidden: Boolean
}

private[maze] class WeaveMaze(val grid: RectGrid, val graph: Graph,
                              hidden: List[Position2D]) extends Maze[Cell2DWeave] {
  private val helper = new MazeHelper[Cell2DWeave, RectGrid](p => Cell2DOverlay(p), grid, graph)

  def this(rows: Int, cols: Int) = this(RectGrid(rows, cols), new Graph(), Nil)

  override def at(position: Position2D): Option[Cell2DWeave] = helper.cells.get(position)

  override def linkedBy(position: Position2D): List[Cell2DWeave] = helper.linkedBy(position)

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = WeaveMazeInfo(grid, this)

  override def randomCell(r: Random): Cell2DWeave = {
    val overlayCells = helper.cells.values.toArray
    val id = r.nextInt(overlayCells.length)
    overlayCells(id)
  }

  private def checkHiddenCellRequired(pos1: Position2D,
                                      pos2: Position2D): Option[Position2D] = {
    if (!grid.isValid(pos1) || !grid.isValid(pos2)) return None
    val c1 = helper.cells(pos1).asInstanceOf[Cell2DOverlay]
    val c2 = helper.cells(pos2).asInstanceOf[Cell2DOverlay]

    if (c1.canTunnelSouth && c1.south.fold(false)(s => s.pos == c2.pos))
      Some(Position2D(pos1.row+1, pos1.col))
    else if (c1.canTunnelNorth && c1.north.fold(false)(n => n.pos == c2.pos))
      Some(Position2D(pos1.row-1, pos1.col))
    else if (c1.canTunnelWest && c1.west.fold(false)(w => w.pos == c2.pos))
      Some(Position2D(pos1.row, pos1.col-1))
    else if (c1.canTunnelEast && c1.east.fold(false)(e => e.pos == c2.pos))
      Some(Position2D(pos1.row, pos1.col+1))
    else None
  }

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DWeave]] = {
    val optPos = checkHiddenCellRequired(pos1, pos2)
    optPos match {
      case Some(p) =>  {
        Some(new WeaveMaze(grid, graph.link(pos1, pos2), hiddenCells.keys.toList :+ p))
      }
      case None => {
        helper.link(pos1, pos2).map(g => new WeaveMaze(grid, g, hiddenCells.keys.toList))
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
      container.at(pos.row-2, pos.col).isDefined && super.north.fold(false)(n => n.isHorizontalLinked)

    def canTunnelSouth: Boolean =
      container.at(pos.row+2, pos.col).isDefined && super.south.fold(false)(s => s.isHorizontalLinked)

    def canTunnelEast: Boolean =
      container.at(pos.row, pos.col+2).isDefined && super.east.fold(false)(e => e.isVerticalLinked)

    def canTunnelWest: Boolean =
      container.at(pos.row, pos.col-2).isDefined && super.west.fold(false)(w => w.isVerticalLinked)

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
      if (canTunnelNorth) container.at(pos.row-2, pos.col)
      else super.north
    }

    override def south: Option[Cell2DWeave] = {
      if (canTunnelSouth) container.at(pos.row+2, pos.col)
      else super.south
    }

    override def east: Option[Cell2DWeave] = {
      if (canTunnelEast) container.at(pos.row, pos.col+2)
      else super.east
    }

    override def west: Option[Cell2DWeave] = {
      if (canTunnelWest) container.at(pos.row, pos.col-2)
      else super.west
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
