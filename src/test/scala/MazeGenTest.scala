import algorithm.{BinaryTreeMaze, DistanceEx, SidewinderMaze}
import com.sksamuel.scrimage.nio.PngWriter
import grid.{CellEx, GridEx}
import org.scalatest.FunSuite

import scala.util.Random

class MazeGenTest extends FunSuite {
  private implicit val writer: PngWriter = PngWriter.MaxCompression
  private val ext = ".png"
  val rand: Random = new Random(System.currentTimeMillis())

  test("Algorithm.BinaryTreeEx") {
    val grid = new GridEx(8,10)
    val generator = new BinaryTreeMaze(rand)
    val maze = generator.generate(grid)
    print(maze)
  }

  test("Algorithm.SidewinderEx") {
    val grid = new GridEx(8,10)
    val generator = new SidewinderMaze(rand)
    val maze = generator.generate(grid)
    val distMap = DistanceEx.from(maze, grid(7, 0)).get
    val content = (cell: CellEx) => {
      val d = distMap(cell)
      val sb = new StringBuilder()
      sb.append(d)
      while (sb.length() < 3) sb.append(' ')
      sb.toString()
    }
    print(maze/*.dump(content)*/); println()
    val goal = grid(7, 5)
    val path = distMap.pathTo(goal)
    if (path.isDefined) {
      val pathSet = path.get.toSet;
      print(maze.dump((c: CellEx) =>
        if (pathSet.contains(c)) content(c) else " x " ))
    }
  }
}
