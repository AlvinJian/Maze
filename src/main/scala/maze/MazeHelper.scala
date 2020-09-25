package maze

private[maze] class MazeHelper[T <: Cell2D, G <: PlainGrid[Position2D]](val f: Position2D => T,
                                                                        val grid: G,
                                                                        val graph: Graph)
  extends Iterable[T] {

  val cells: Map[Position2D, T] = grid.map(p => (p, f(p))).toMap

  def link(pos1: Position2D, pos2: Position2D): Option[Graph] = {
    if (grid.isValid(pos1) && grid.isValid(pos2)) Some(graph.link(pos1, pos2)) else None
  }

  def linkedBy(position: Position2D): List[T] = graph.linked(position).map(p => cells.apply(p)).toList

  override def iterator: Iterator[T] = cells.values.toList.sortBy(c => c.pos).iterator
}
