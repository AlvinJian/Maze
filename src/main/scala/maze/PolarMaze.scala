package maze

sealed trait Cell2DPolar extends Cell2D {
  override type T = Cell2DPolar
  def cw: T
  def ccw: T
  def outward: List[T]
  def inward: Option[T]
  def neighbors: List[T] = {
    val candidates: List[T] = List(cw, ccw) ++ outward ++ inward
    if (pos.row == 0) candidates.filter(c=>c!=this) // when r == 0, this is required!
    else candidates
  }
}

private[maze] class PolarMaze(val grid: PolarGrid, val graph: Graph = new Graph()) extends Maze[Cell2DPolar] {
  private val helper = new MazeHelper[Cell2DPolar, PolarGrid](this, grid, graph)
  private val cells: Map[Position2D, Cell2DPolar] = grid.map(p=>(p, new Cell2DPolarImpl(p))).toMap

  def this(radius: Int) = {
    this(PolarGrid(radius))
  }

  override def at(position: Position2D): Option[Cell2DPolar] = {
    val normPos = grid.at(position.row, position.col).get
    cells.get(normPos)
  }

  override def linkedBy(position: Position2D): List[Cell2DPolar] = helper.linkedBy(position)

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = PolarMazeInfo(grid, this)

  override def link(pos1: Position2D, pos2: Position2D): Option[Maze[Cell2DPolar]] = {
    helper.link(pos1, pos2) match {
      case Some(newGraph) => Some(new PolarMaze(grid, newGraph))
      case None => None
    }
  }

  override def iterator: Iterator[Cell2DPolar] = cells.valuesIterator

  private class Cell2DPolarImpl(position: Position2D) extends Cell2DPolar {
    override def container: Maze[Cell2DPolar] = PolarMaze.this
    override def pos: Position2D = position

    override def cw: Cell2DPolar = container.at(pos.row, pos.col-1).get

    override def ccw: Cell2DPolar = container.at(pos.row, pos.col+1).get

    override def outward: List[Cell2DPolar] = {
      val grid: PolarGrid = PolarMaze.this.grid
      if (pos.row == 0) container.filter(c=>c.pos.row == 1).toList
      else if (pos.row < grid.radius-1) {
        val count = grid.countAt(pos.row)
        val outerCount = grid.countAt(pos.row+1)
        if (outerCount > count)
          List(container.at(pos.row+1, 2*pos.col), container.at(pos.row+1, 2*pos.col+1)).flatten
        else List(container.at(pos.row+1, pos.col)).flatten
      } else Nil
    }

    override def inward: Option[Cell2DPolar] = {
      val grid: PolarGrid = PolarMaze.this.grid
      if (pos.row == 0) None
      else if (pos.row == 1) container.at(0,0)
      else {
        val count = grid.countAt(pos.row)
        val innerCount = grid.countAt(pos.row-1)
        if (count == innerCount) container.at(pos.row-1, pos.col)
        else container.at(pos.row-1, pos.col/2)
      }
    }
  }
}

object PolarMaze {
  def apply(radius: Int): Maze[Cell2DPolar] = new PolarMaze(radius)
}
