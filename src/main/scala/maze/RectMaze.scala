package maze

sealed trait Cell2DRect extends Cell2D {
  override type T = Cell2DRect
  def north: Option[T]
  def south: Option[T]
  def east: Option[T]
  def west: Option[T]
  override def neighbors: List[T] = List(this.north, this.south, this.east, this.west).flatten
}

private[maze] class RectMaze(val grid: RectGrid, val graph: Graph = new Graph()) extends Maze[Cell2DRect] {
  private val helper = new MazeHelper[Cell2DRect, RectGrid](this, p => new Cell2DRectImpl(p),
                                                            grid, graph)

  def this(rows: Int, cols: Int) = {
    this(RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = helper.cells.get(pos)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DRect]] = {
    helper.link(pos1, pos2) match {
      case Some(newGraph) => Some(new RectMaze(grid, newGraph))
      case None => None
    }
  }

  override def iterator: Iterator[Cell2DRect] = helper.iterator

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = RectMazeInfo(grid, this)

  override def linkedBy(position: Position2D): List[Cell2DRect] = helper.linkedBy(position)

  private class Cell2DRectImpl(position2D: Position2D) extends Cell2DRect {
    override def container: Maze[Cell2DRect] = RectMaze.this

    override def pos: Position2D = position2D

    override def north: Option[Cell2DRect] = {
      val row = pos.row-1
      val col = pos.col
      container.at(Position2D(row, col))
    }

    override def south: Option[Cell2DRect] = {
      val row = pos.row+1
      val col = pos.col
      container.at(Position2D(row, col))
    }

    override def east: Option[Cell2DRect] = {
      val row = pos.row
      val col = pos.col+1
      container.at(Position2D(row, col))
    }

    override def west: Option[Cell2DRect] = {
      val row = pos.row
      val col = pos.col-1
      container.at(Position2D(row, col))
    }
  }
}

object RectMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DRect] = new RectMaze(rows, cols)
}
