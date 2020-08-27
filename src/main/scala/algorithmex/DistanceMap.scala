package algorithmex

import maze.{Cell2D, Maze, MazeInfo, PlainGrid, Position2D}

import scala.annotation.tailrec

trait DistanceMap {
  def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]]
  def data: Map[Position2D, Int]
  def root: Position2D
  def max: (Position2D, Int)
  def pathTo(pos: Position2D): List[Position2D]
}

private class DistanceMapImp(i: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]],
                             m: Map[Position2D, Int]) extends DistanceMap {
  private val _max = data.maxBy(tup=>tup._2)
  private val _root = data.find(tup=>tup._2 == 0).get._1

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = i

  override def data: Map[Position2D, Int] = m

  override def root: Position2D = _root

  override def max: (Position2D, Int) = _max

  override def pathTo(pos: Position2D): List[Position2D] = {
    if (!data.contains(pos)) return Nil
    @tailrec
    def loop(queue: List[List[Position2D]]): List[Position2D] = queue match {
      case ::(firstPath, remain) => {
        if (firstPath.head != root) {
          val cur = firstPath.head
          val nextValue = data.apply(cur)-1
          val candidates = info.maze.linkedBy(cur).filter(c => data.apply(c.pos) == nextValue)
          loop(remain ++ candidates.map(c => c.pos +: firstPath))
        } else firstPath
      }
      case Nil => Nil
    }
    loop(List(List(pos)))
  }
}

object DistanceMap {
  def tryCreate(start: Position2D, maze: Maze[Cell2D]): Option[DistanceMap] = {
    if (maze.at(start).isEmpty) return None
    @tailrec
    def loop(queue: List[Position2D], prev: Map[Position2D, Int]): Map[Position2D, Int] = queue match {
      case ::(head, next) => {
        val cur = head
        val nextValue = prev.apply(cur)+1
        val candidates = maze.linkedBy(cur).filter(c => !prev.contains(c.pos))
        val newMap = prev ++ candidates.map(c => (c.pos, nextValue))
        loop(next ++ candidates.map(c => c.pos), newMap)
      }
      case Nil => prev
    }
    val data = loop(List(start), Map(start -> 0))
    Some(new DistanceMapImp(maze.info, data))
  }
  def maximize(seed: Position2D, maze: Maze[Cell2D]): Option[DistanceMap] = {
    tryCreate(seed, maze) match {
      case Some(tmp) => {
        val (newStart, _) = tmp.max
        tryCreate(newStart, maze)
      }
      case None => None
    }
  }
}
