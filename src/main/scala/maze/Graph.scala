package maze

private[maze] class Graph(val map:Map[Position2D, Set[Position2D]] = Map()) {
  def link(pos1: Position2D, pos2: Position2D): Graph = {
    var newMap = map
    newMap = Graph.linkHelper(newMap, pos1, pos2)
    newMap = Graph.linkHelper(newMap, pos2, pos1)
    new Graph(newMap)
  }

  def linked(pos: Position2D): Set[Position2D] = map.getOrElse(pos, Set())
}

object Graph {
  private def linkHelper(iMap: Map[Position2D, Set[Position2D]],
                         p1: Position2D, p2: Position2D): Map[Position2D, Set[Position2D]] = {
    if (iMap.contains(p1)) iMap.updated(p1, iMap(p1) + p2)
    else iMap + (p1 -> Set(p2))
  }
}
