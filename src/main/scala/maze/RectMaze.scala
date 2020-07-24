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

case class RectMazeDimension(rows: Int, cols: Int, maze: Maze[Cell2DRect]) extends MazeDimension

private[maze] class RectMaze(grid: RectGrid, graph:Graph = new Graph()) extends Maze[Cell2DRect] {
  protected val defaultMazeImpl = new DefaultMazeImpl[Cell2DRect, RectGrid](grid, graph,
    (p: Position2D)=>new Cell2DRectImpl(p), (g: RectGrid, gr: Graph)=>new RectMaze(g, gr),
    ()=>this.dimension)

  def this(rows: Int, cols: Int) {
    this(RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = defaultMazeImpl.at(pos)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DRect]] = defaultMazeImpl.link(pos1, pos2)

  class Cell2DRectImpl(position2D: Position2D) extends Cell2DRect {
    protected val outer: RectMaze = RectMaze.this

    override def container: Maze[Cell2DRect] = outer

    override def pos: Position2D = position2D

    override def linkedCells: Set[Cell2DRect] = {
      defaultMazeImpl.linked(this.pos).map(p => defaultMazeImpl.at(p).get)
    }
  }

  override def iterator: Iterator[Cell2DRect] = defaultMazeImpl.iterator

  override def dimension: MazeDimension = RectMazeDimension(grid.rows, grid.cols, this)

  override def linked(pos: Position2D): Set[Position2D] = defaultMazeImpl.linked(pos)
}

object RectMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DRect] = new RectMaze(rows, cols)
}
