package algorithmex
import maze.{Cell2DRect, Maze, Position2D, RectMazeInfo}

import scala.annotation.tailrec
import scala.util.Random

object SidewinderMaze extends MazeGenerator {
  override type C = Cell2DRect

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    val grid = maze.info.asInstanceOf[RectMazeInfo].grid
    var tmpMaze = maze
    for(r <- 0.until(grid.rows)) {
      var run = Vector[Position2D]()
      for (c <- 0.until(grid.cols)) {
        run = run :+ Position2D(r, c)
        val isEastEnd = tmpMaze.at(r, c).get.east.isEmpty
        val isNorthEnd = tmpMaze.at(r, c).get.north.isEmpty
        val shouldClose = isEastEnd || (!isNorthEnd && random.nextInt(2) == 1)
        if (shouldClose) {
          val placeToNorth = run(random.nextInt(run.size))
          if (tmpMaze.at(placeToNorth).get.north.isDefined) {
            val otherPos = tmpMaze.at(placeToNorth).get.north.get.pos
            tmpMaze = tmpMaze.link(placeToNorth, otherPos) match {
              case Some(value) => value
              case None => tmpMaze
            }
          }
          run = Vector[Position2D]()
        } else if (tmpMaze.at(r,c).get.east.isDefined) {
          val otherPos = tmpMaze.at(r,c).get.east.get.pos
          tmpMaze = tmpMaze.link(Position2D(r, c), otherPos) match {
            case Some(value) => value
            case None => tmpMaze
          }
        }
      }
    }
    tmpMaze
  }
}
