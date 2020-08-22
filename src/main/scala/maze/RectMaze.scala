package maze

sealed trait Cell2DRect extends Cell2D {
  override type T = Cell2DRect
  def north: Option[T] = {
    val row = pos.row-1
    val col = pos.col
    container.at(Position2D(row, col))
  }
  def south: Option[T] = {
    val row = pos.row+1
    val col = pos.col
    container.at(Position2D(row, col))
  }
  def east: Option[T] = {
    val row = pos.row
    val col = pos.col+1
    container.at(Position2D(row, col))
  }
  def west: Option[T] = {
    val row = pos.row
    val col = pos.col-1
    container.at(Position2D(row, col))
  }
  override def neighbors: List[T] = List(this.north, this.south, this.east, this.west).flatten
}

private class Cell2DRectImpl(position2D: Position2D, maze: Maze[Cell2DRect]) extends Cell2DRect {
  override def container: Maze[Cell2DRect] = maze

  override def pos: Position2D = position2D

  override def linked: List[Cell2DRect] = container.linkedBy(this.pos)
}

private[maze] class RectMaze(val grid: RectGrid, val graph: Graph = new Graph()) extends Maze[Cell2DRect] {
  private val helper = new MazeHelper[Cell2DRect, RectGrid](this, grid, graph)
  private val cells = helper.buildCells((p, maze)=>new Cell2DRectImpl(p, maze))

  def this(rows: Int, cols: Int) = {
    this(RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = cells.get(pos)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DRect]] = {
    helper.link(pos1, pos2) match {
      case Some(newGraph) => Some(new RectMaze(grid, newGraph))
      case None => None
    }
  }

  override def iterator: Iterator[Cell2DRect] = cells.valuesIterator

  override def info: MazeInfo = RectMazeInfo(grid, this)

  override def linkedBy(position: Position2D): List[Cell2DRect] = helper.linkedBy(position)
}

object RectMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DRect] = new RectMaze(rows, cols)
}
