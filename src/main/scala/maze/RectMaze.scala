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

  override def linkedCells: Set[Cell2DRect] = {
    container.linked(this.pos).map(p => container.at(p).get)
  }
}

case class RectMazeInfo(override val grid: RectGrid,
                        override val maze: Maze[Cell2DRect])
  extends RichMazeInfo[RectGrid, Maze[Cell2DRect]] {
  override val name: String = "RectMaze"
}

private[maze] class RectMaze(grid: RectGrid, graph:Graph = new Graph()) extends Maze[Cell2DRect] {
  protected val defaultMazeImpl = new DefaultMazeImpl[Cell2DRect, RectGrid](grid, graph,
    (p, maze)=>new Cell2DRectImpl(p, maze), (g: RectGrid, gr: Graph)=>new RectMaze(g, gr),
    ()=>this.info)

  def this(rows: Int, cols: Int) {
    this(RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = defaultMazeImpl.at(pos)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DRect]] = defaultMazeImpl.link(pos1, pos2)

  override def iterator: Iterator[Cell2DRect] = defaultMazeImpl.iterator

  override def info: MazeInfo = RectMazeInfo(grid, this)

  override def linked(pos: Position2D): Set[Position2D] = defaultMazeImpl.linked(pos)
}

object RectMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DRect] = new RectMaze(rows, cols)
}
