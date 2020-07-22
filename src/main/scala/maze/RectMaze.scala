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
  protected lazy val cells: Map[Position2D,Cell2DRect] = grid.data.map(p=>(p,new Cell2DRectImpl(p))).toMap

  def this(rows: Int, cols: Int) {
    this(new RectGrid(rows, cols))
  }

  override def at(pos: Position2D): Option[Cell2DRect] = {
    grid.at(pos.row, pos.col) match {
      case Some(pos) =>  Some(cells.apply(pos))
      case None => None
    }
  }

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DRect]] = {
    if (grid.isValid(pos1) && grid.isValid(pos2)) {
      val newGraph = graph.link(pos1, pos2)
      Some(RectMaze(grid, newGraph))
    } else None
  }

  class Cell2DRectImpl(position2D: Position2D) extends Cell2DRect {
    protected val outer: RectMaze = RectMaze.this

    override def container: Maze[Cell2DRect] = outer

    override def pos: Position2D = position2D

    override def linkedCells: Set[Cell2DRect] = {
      outer.graph.linked(this.pos).map(p => outer.cells.apply(p))
    }
  }

  override def iterator: Iterator[Cell2DRect] = cells.valuesIterator

  override def dimension: MazeDimension = RectMazeDimension(grid.rows, grid.cols)
}
