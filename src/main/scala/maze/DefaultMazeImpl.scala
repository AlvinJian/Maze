package maze

private[maze] class DefaultMazeImpl[T <: Cell2D, G <: PlainGrid[Position2D]]
                                   (val grid: G, val graph: Graph,
                                    f: (Position2D, Maze[T])=>T,
                                    selfBuilder: (G, Graph)=>Maze[T],
                                    dimDef: ()=>MazeInfo) extends Maze[T] {
  protected val cells: Map[Position2D, T] = grid.map( p=>(p, f(p, this)) ).toMap

  def at(pos: Position2D): Option[T] = cells.get(pos)

  def link(pos1: Position2D, pos2: Position2D): Option[Maze[T]] = {
    if (grid.isValid(pos1) && grid.isValid(pos2)) {
      val newGraph = graph.link(pos1, pos2)
      Some(selfBuilder(grid, newGraph))
    } else None
  }

  def iterator: Iterator[T] = cells.valuesIterator

  override def info: MazeInfo = dimDef()

  override def linked(pos: Position2D): Set[Position2D] = graph.linked(pos)
}
