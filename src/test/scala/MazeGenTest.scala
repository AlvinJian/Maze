import algorithm.{BinaryTreeMaze, DistanceEx, SidewinderMaze}
import com.sksamuel.scrimage.nio.PngWriter
import grid.{CellEx, GraphEx, GridEx}
import org.scalatest.FunSuite
import utils.FileHelper

import scala.util.Random

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())

  def pathCheck(path: List[CellEx], distMap: DistanceEx,
                start: CellEx, end: CellEx): Unit = {
    assert(path.head === start)
    assert(path.last === end)
    val graph = distMap.graph
    for (i <- 0 until path.size-1) {
      assert(graph.isLinked(path(i), path(i+1)))
      assert(distMap(path(i))+1 === distMap(path(i+1)))
    }
  }

  test("Algorithm.BinaryTreeEx") {
    val grid = new GridEx(8,10)
    val generator = new BinaryTreeMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
    val start = grid(7, 0)
    val distMap = DistanceEx.from(maze, start).get
    val end = grid(7, 5)
    distMap.pathTo(end) match {
      case Some(path) => pathCheck(path, distMap, start, end)
      case None => assert(false)
    }
    val img = maze.asImage(32)
    FileHelper.saveToFile(img, writer, s"BinaryTree${ext}", "images")
  }

  test("Algorithm.SidewinderEx") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val start: CellEx = grid(7, 0)
    val distMap = DistanceEx.from(maze, start).get
    val content = (cell: CellEx) => {
      val d = distMap(cell)
      val sb = new StringBuilder()
      sb.append(d)
      while (sb.length() < 3) sb.append(' ')
      sb.toString()
    }
    print(maze/*.dump(content)*/); println()
    val img = maze.asImage(32)
    FileHelper.saveToFile(img, writer, s"Sidewinder$ext", "images")
    val goal = grid(7, 5)
    distMap.pathTo(goal) match {
      case Some(path) => {
        val pathSet = path.toSet;
        print(maze.dump((c: CellEx) =>
          if (pathSet.contains(c)) content(c) else " x " ))
        pathCheck(path, distMap, start, goal)
      }
      case None => assert(false)
    }
  }
}
