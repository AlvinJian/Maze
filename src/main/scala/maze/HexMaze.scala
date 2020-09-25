package maze

trait Cell2DHex extends Cell2D {
  override type T = Cell2DHex
  def northeast: Option[T]
  def north: Option[T]
  def northwest: Option[T]
  def southwest: Option[T]
  def south: Option[T]
  def southeast: Option[T]
  override def neighbors: List[T] = List(northeast, north, northwest,
    southwest, south, southeast).flatten
}

private[maze] class HexMaze(val grid: RectGrid, val graph: Graph = new Graph()) extends Maze[Cell2DHex] {
  private val helper = new MazeHelper[Cell2DHex, RectGrid](p => new Cell2DHexImpl(p), grid, graph)

  def this(rows: Int, cols: Int) = {
    this(RectGrid(rows, cols))
  }

  override def at(position: Position2D): Option[Cell2DHex] = helper.cells.get(position)

  override def linkedBy(position: Position2D): List[Cell2DHex] = helper.linkedBy(position)

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = HexMazeInfo(grid, this)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DHex]] = {
    helper.link(pos1, pos2).map(newGraph => new HexMaze(grid, newGraph))
  }

  override def iterator: Iterator[Cell2DHex] = helper.iterator

  private class Cell2DHexImpl(position2D: Position2D) extends Cell2DHex {
    private def northDiagonalRow: Int = if (pos.col % 2  == 0) pos.row-1 else pos.row
    private def southDiagonalRow: Int = if (pos.col % 2 == 0) pos.row else pos.row + 1

    override def northeast: Option[Cell2DHex] = container.at(northDiagonalRow, pos.col + 1)

    override def north: Option[Cell2DHex] = container.at(pos.row-1, pos.col)

    override def northwest: Option[Cell2DHex] = container.at(northDiagonalRow, pos.col-1)

    override def southwest: Option[Cell2DHex] = container.at(southDiagonalRow, pos.col-1)

    override def south: Option[Cell2DHex] = container.at(pos.row+1, pos.col)

    override def southeast: Option[Cell2DHex] = container.at(southDiagonalRow, pos.col+1)

    override def container: Maze[Cell2DHex] = HexMaze.this

    override def pos: Position2D = position2D
  }
}

object HexMaze {
  def apply(rows: Int, cols: Int): Maze[Cell2DHex] = new HexMaze(rows, cols)
}
