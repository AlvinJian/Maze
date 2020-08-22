package maze

private[maze] class MazeHelper[T <: Cell2D, G <: PlainGrid[Position2D]](val maze: Maze[T],
                                                                        val grid: G,
                                                                        val graph: Graph) {
  def buildCells(f: (Position2D, Maze[T])=>T): Map[Position2D, T] = grid.map(p=>(p, f(p, maze))).toMap

  def link(pos1: Position2D, pos2: Position2D): Option[Graph] = {
    if (grid.isValid(pos1) && grid.isValid(pos2)) Some(graph.link(pos1, pos2)) else None
  }

  def linkedBy(position: Position2D): List[T] = graph.linked(position)
    .flatMap(p => maze.at(p)).toList
}
