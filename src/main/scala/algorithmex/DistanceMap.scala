package algorithmex

import maze.{Cell2D, Maze, MazeInfo, PlainGrid, Position2D}

import scala.annotation.tailrec

trait DistanceMap {
  def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]]
  def data: Map[Position2D, Int]
  def root: Position2D
  def max: (Position2D, Int)
  def pathTo(dest: Position2D): List[Position2D]
}

private class DistanceMapImp(i: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]],
                             m: Map[Position2D, Int]) extends DistanceMap {
  private val _max = data.maxBy(tup=>tup._2)
  private val _root = data.find(tup=>tup._2 == 0).get._1

  override def info: MazeInfo[PlainGrid[Position2D], Maze[Cell2D]] = i

  override def data: Map[Position2D, Int] = m

  override def root: Position2D = _root

  override def max: (Position2D, Int) = _max

  override def pathTo(dest: Position2D): List[Position2D] = {
    if (!data.contains(dest)) return Nil
    @tailrec
    def loop(queue: List[List[Position2D]]): List[Position2D] = queue match {
      case ::(firstPath, remain) => {
        if (firstPath.head != root) {
          val cur = firstPath.head
          val nextValue = data(cur)-1
          val candidates = info.maze.linkedBy(cur).filter(c => data(c.pos) == nextValue).map(c => c.pos)
          loop(remain ++ candidates.map(c => c +: firstPath))
        } else firstPath
      }
      case Nil => Nil
    }
    loop(List(List(dest)))
  }
}

object DistanceMap {
  def tryCreate(start: Position2D, maze: Maze[Cell2D]): Option[DistanceMap] = {
    @tailrec
    def loop(queue: List[Position2D], prev: Map[Position2D, Int]): Map[Position2D, Int] = queue match {
      case ::(head, next) => {
        val cur = head
        val nextValue = prev.apply(cur)+1
        val candidates = maze.linkedBy(cur).filter(c => !prev.contains(c.pos)).map(c => c.pos)
        val newMap = prev ++ candidates.map(c => (c, nextValue))
        loop(next ++ candidates, newMap)
      }
      case Nil => prev
    }
    maze.at(start).map {
      c => {
        val data = loop(List(c.pos), Map(c.pos -> 0))
        new DistanceMapImp(maze.info, data)
      }
    }
  }
  def maximize(seed: Position2D, maze: Maze[Cell2D]): Option[DistanceMap] = {
    tryCreate(seed, maze).flatMap {
      tmp => {
        val (newStart, _) = tmp.max
        tryCreate(newStart, maze)
      }
    }
  }
}
