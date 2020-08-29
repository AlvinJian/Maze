package maze

sealed trait Cell2DTriangle extends Cell2D {
  override type T = Cell2DTriangle
  def north: Option[T]
  def south: Option[T]
  def east: Option[T]
  def west: Option[T]
  override def neighbors: List[T] = List(this.north, this.south, this.east, this.west).flatten
  def isUpright: Boolean
}

private[maze] class TriangleMaze(val grid: RectGrid, val graph: Graph = new Graph()) extends Maze[Cell2DTriangle] {
  private val helper = new MazeHelper[Cell2DTriangle, RectGrid](this, p => new Cell2DTriangleImpl(p),
                                                                grid, graph)
  def this(rows: Int, cols: Int) = {
    this(RectGrid(rows, cols))
  }

  override def at(position: Position2D): Option[Cell2DTriangle] = helper.cells.get(position)

  override def linkedBy(position: Position2D): List[Cell2DTriangle] = helper.linkedBy(position)

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = TriangleMazeInfo(grid, this)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DTriangle]] = {
    helper.link(pos1, pos2) match {
      case Some(newGraph) => Some(new TriangleMaze(grid, newGraph))
      case None => None
    }
  }

  override def iterator: Iterator[Cell2DTriangle] = helper.iterator

  private class Cell2DTriangleImpl(p: Position2D) extends Cell2DTriangle {
    override def north: Option[Cell2DTriangle] = {
      if (!isUpright) {
        val (row, col) = (pos.row-1, pos.col)
        container.at(Position2D(row, col))
      } else None
    }

    override def south: Option[Cell2DTriangle] = {
      if (isUpright) {
        val (row, col) = (pos.row+1, pos.col)
        container.at(Position2D(row, col))
      } else None
    }

    override def east: Option[Cell2DTriangle] = {
      val row = pos.row
      val col = pos.col+1
      container.at(Position2D(row, col))
    }

    override def west: Option[Cell2DTriangle] = {
      val row = pos.row
      val col = pos.col-1
      container.at(Position2D(row, col))
    }

    override def isUpright: Boolean = (pos.row + pos.col) % 2 == 0

    override def container: Maze[Cell2DTriangle] = TriangleMaze.this

    override def pos: Position2D = p
  }
}

object TriangleMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DTriangle] = new TriangleMaze(rows, cols)
}
