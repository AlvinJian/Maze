package maze

trait Cell2DRect extends Cell2D {
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

case class RectMazeDimension(rows: Int, cols: Int) extends MazeDimension

case class RectMaze(grid: RectGrid, graph:Graph = new Graph()) extends Maze[Cell2DRect] {
  def this(rows: Int, cols: Int) {
    this(new RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = {
    grid.at(pos.row, pos.col) match {
      case Some(value) =>  Some(new Cell2DRectImpl(value))
      case None => None
    }
  }

  class Cell2DRectImpl(position2D: Position2D) extends Cell2DRect {
    protected val outer: RectMaze = RectMaze.this

    override def container: Maze[Cell2DRect] = outer

    override def pos: Position2D = position2D

    override def link(other: Cell2DRect): Option[Maze[this.T]] = {
      if (container == other.container) {
        val newGraph = graph.link(this.pos, other.pos)
        Some(outer.copy(outer.grid, graph = newGraph))
      } else None
    }

    override def linkedCells: Set[Cell2DRect] =
      outer.graph.linked(this.pos).map(p => new Cell2DRectImpl(p)).toSet
  }

  override def iterator: Iterator[Cell2DRect] = new Iterator[Cell2DRect] {
    private val _iterator = RectMaze.this.grid.data.iterator

    override def hasNext: Boolean = _iterator.hasNext
    override def next(): Cell2DRect = new Cell2DRectImpl(_iterator.next())
  }

  override def dimension: MazeDimension = RectMazeDimension(grid.rows, grid.cols)
}