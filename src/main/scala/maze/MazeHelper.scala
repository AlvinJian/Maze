package maze

private[maze] class MazeHelper[T <: Cell2D, G <: PlainGrid[Position2D]](val maze: Maze[T],
                                                                        val f: Position2D => T,
                                                                        val grid: G,
                                                                        val graph: Graph)
  extends Iterable[T] {
  private val _cells = grid.map(p => (p, f(p))).toMap

  def cells: Map[Position2D, T] = _cells

  def link(pos1: Position2D, pos2: Position2D): Option[Graph] = {
    if (grid.isValid(pos1) && grid.isValid(pos2)) Some(graph.link(pos1, pos2)) else None
  }

  def linkedBy(position: Position2D): List[T] = graph.linked(position).flatMap(p => maze.at(p)).toList

  override def iterator: Iterator[T] = cells.values.toList.sortBy(c => c.pos).iterator
}
