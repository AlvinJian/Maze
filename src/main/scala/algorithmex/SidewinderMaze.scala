package algorithmex
import maze.{Cell2DRect, Maze, Position2D, RectMazeInfo}

import scala.annotation.tailrec
import scala.util.Random

object SidewinderMaze extends MazeGenerator {
  override type C = Cell2DRect

  override def generate[CC <: C](random: Random, maze: Maze[CC]): Maze[CC] = {
    val positions = maze.map(c => c.pos)
    val (genMaze, _) = positions.foldLeft((maze, Vector[Position2D]()))((tup, p) => {
      val (tmpMaze, prevRun) = tup
      val run: Vector[Position2D] = {
        if (p.col == 0) Vector(p)
        else prevRun :+ p
      }
      val isEastEnd = tmpMaze.at(p).flatMap(c => c.east).isEmpty
      val isNorthEnd = tmpMaze.at(p).flatMap(c => c.north).isEmpty
      val shouldClose = isEastEnd || (!isNorthEnd && random.nextInt(2) == 1)
      if (shouldClose) {
        val placeToNorth = run(random.nextInt(run.size))
        val optNewMaze = tmpMaze.at(placeToNorth).flatMap(c => c.north).flatMap(north =>
          tmpMaze.link(placeToNorth, north.pos))
        optNewMaze match {
          case Some(value) => (value, Vector[Position2D]())
          case None => (tmpMaze, Vector[Position2D]())
        }
      } else {
        val optNewMaze = tmpMaze.at(p).flatMap(c => c.east).flatMap(east => tmpMaze.link(p, east.pos))
        optNewMaze match {
          case Some(value) => (value, run)
          case None => (tmpMaze, run)
        }
      }
    })
    genMaze
  }
}
