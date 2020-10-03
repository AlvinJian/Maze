package algorithmex

import maze.{Cell2D, Maze, Position2D}

import scala.annotation.tailrec
import scala.util.Random

trait MazeGenerator {
  type C <: Cell2D
  def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC]
}

object MazeGenerator {
  def braid[CC <: Cell2D](random: Random, maze: Maze[CC], param: Double = 1.0): Maze[CC] = {
    val deadEnds: Array[Position2D] =  maze.filter(c => c.linked.size == 1).map(c => c.pos).toArray
    val indices: Array[Int] = deadEnds.indices.toArray
    val randIndices = random.shuffle(indices)
    @tailrec
    def loop(cur: Int, m: Maze[CC]): Maze[CC] = {
      if (cur == randIndices.length) return m
      val curIndex = randIndices(cur)
      val curPos = deadEnds(curIndex)
      if (m.at(curPos).fold(0)(c => c.linked.size) != 1 || random.nextDouble() > param) {
        loop(cur+1, m)
      } else {
        val neighbors = m.neighborsAt(curPos)
        val candidates = neighbors.filter(
          nc => m.at(curPos).fold(false)(curCell => !curCell.linked.contains(nc)))
        val bestCandidates = candidates.filter(c => c.linked.size == 1)
        val best = {
          if (bestCandidates.nonEmpty) bestCandidates(random.nextInt(bestCandidates.size))
          else candidates(random.nextInt(candidates.size))
        }
        loop(cur+1, m.link(curPos, best.pos).fold(m)(mm => mm))
      }
    }
    loop(0, maze)
  }
}
